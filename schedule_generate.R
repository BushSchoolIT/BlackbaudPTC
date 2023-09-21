library(tidyverse)
library(data.table)

#  Primary function is *schedule_gen*
#  Other functions for loading in data
#  And functions for running schedule_gen in parallel

run_date <- "9132022"
stochastic_output_folder <- "2022-2023 Sims v2"
number_of_sims <- 30000

# Schedule Phelps only on Day 1 and Day 2
# Put siblings next to each other for parents

# Predetermined Fall-Spring
semester_fixed <- data.table(
  Student = c("Ava Armbruster", "Cole Masters", "Eve Hinderliter", "Geoffrey Arone", 
              "Lauren Eva", "Maile Krueger", "Makie Koizumi-Hachey"),
  Semester = rep("Fall Semester", times = 7),
  key = "Student"
)

full_schedule <- expand_grid(
  Semester = c("Fall Semester", "Spring Semester"),
  Day = 1:3,
  Time = 1:13,
  Room = c("9th Grade", "10th Grade", "11th Grade")
) %>% data.table
full_schedule[,
  Room := factor(Room, 
    levels = c("9th Grade", "10th Grade", "11th Grade"),
    ordered = TRUE
  )
]
setkeyv(full_schedule, c("Semester", "Day", "Time", "Room"))

time <- unique(full_schedule[,.(Day, Time)], keyby = c("Day", "Time"))
time[,Slot := paste(Day, Time, sep = "_")]

# Import Data (see Conferences/R/data_extract.R)

dt <- readRDS(file = paste0("./Output/conference_", run_date, ".rds")) %>% 
  data.table(., 
    key = c("Group", "GradeLevel", "Student", "Advisor", "Duration", "Teacher")
  )

advisor_student <- unique(
  dt[
    GradeLevel != "12th Grade"    # Remove 12 graders
  ][,.(
    Group, Advisor, Student, GradeLevel  # Select Key
  )], 
  by = c("Advisor", "Group", "Student", "GradeLevel")
) %>% 
  data.table(., key = c("Advisor", "Group", "Student", "GradeLevel"))


# Data Sanity Checks

stopifnot("Unique Grade Levels" = 
  0 == nrow(dt[,.(GradeCount = n_distinct(GradeLevel)), by = "Student"][GradeCount > 1])
)
stopifnot("Unique Advisors" = 
  0 == nrow(dt[,.(AdvisorCount = n_distinct(Advisor)), by = "Student"][AdvisorCount > 1])
)
stopifnot("Unique GradYear" = 
  0 == nrow(dt[,.(GYCount = n_distinct(GradYear)), by = "Student"][GYCount > 1])
)

# Schedule Work ===========================

schedule_gen <- function() {
  
  schedule <- data.table::copy(full_schedule)
  adv_stu <- data.table::copy(advisor_student)
  
  initial_schedule <- function() {

    # Randomly Generate Assigned Conference (Fall or Spring)
    adv_stu[,
      Semester := c(
        rep("Fall Semester", times = floor(.N/2)),
        rep("Spring Semester", times = floor(.N/2)),
        sample(x = c("Fall Semester", "Spring Semester"), size = .N - 2*floor(.N/2), replace = TRUE)
      ),
      keyby = c("Advisor", "GradeLevel")
    ]
    adv_stu[,
      Semester := sample(Semester), # Random Shuffle
      by = c("Advisor", "GradeLevel")
    ]
    setkeyv(adv_stu, c("Semester", "Advisor", "GradeLevel", "Student"))

    # Identify siblings that need to be switched
    adv_stu[,
      Count := n_distinct(Semester),
      keyby = "Group"
    ][,
      Modifiable := fifelse(Count == 1, "Y", sample(c("Y", "N"), size = .N, replace = FALSE)),
      by = "Group"
    ]

    # For "predetermined" students, set the semester and set to unmodifiable
    adv_stu[semester_fixed,
      on = "Student",
      `:=`(
        Semester = i.Semester,
        Modifiable = "N"
      )
    ]

    # Identify siblings that need to be switched
    for (i in seq(nrow(adv_stu))) {
      if (adv_stu[i, Count] != 2) next
      if (adv_stu[i,Modifiable] == "N") next
      advisor_ <- adv_stu[i,Advisor]; grade_level <- adv_stu[i,GradeLevel]
      sem_target <- setdiff(c("Fall Semester", "Spring Semester"), adv_stu[i,Semester])
      tmp <- adv_stu[
        Advisor == advisor_ & Semester == sem_target & Count == 1 & Modifiable == "Y"
      ]
      if (grade_level %in% tmp$GradeLevel) {
        stu_target <- tmp[GradeLevel == adv_stu[i,GradeLevel],.SD[1]][,Student]
      } else {
        stu_target <- tmp$Student[[1]]
      }
      # Perform switch
      if (length(stu_target) > 0) {
        adv_stu[Student == stu_target & Advisor == advisor_,
          Semester := adv_stu[i,Semester]
        ]
      }
      adv_stu[i, Semester := sem_target]
      adv_stu[i, Modifiable := "N"]
    }
    # Cleanup
    adv_stu[,`:=`(
      Count = NULL, Modifiable = NULL
    )]
    rm(i, tmp, advisor_, grade_level, sem_target, stu_target)

    # Randomly Assign initial time slots

    adv_stu[,`:=`(
      TimeSlot = sample(time$Slot, size = .N, replace = TRUE),
      Room = GradeLevel
    ),
      by = c("GradeLevel", "Semester")
    ][,
      c("Day", "Time") := tstrsplit(TimeSlot, "_", fixed = TRUE)
    ][,`:=`(
      Day = as.integer(as.numeric(Day)),
      Time = as.integer(as.numeric(Time)),
      TimeSlot = NULL
    )]

    # Overwrite slots for siblings to be next to each other
    adv_stu <<- adv_stu[order(Group, Time)][,`:=`(
      Day = Day[[1]],
      Time =
        if(Time[[1]] <= 13 - .N + 1) Time[[1]] + (seq(.N)-1)
        else Time[[1]] - (seq(.N, 1) + 1),
      GroupCount = .N
    ),
      keyby = "Group"
    ]
    adv_stu <<- adv_stu[order(-GroupCount, Group, Time)]
    setcolorder(adv_stu, c(
      "Group", "GroupCount", "Student", "GradeLevel", "Advisor",
      "Semester", "Day", "Time", "Room"
    ))
  }

  improve_student_colls <- function() {
    # Update Time slot assignments to reduce conflicts

    # ** Collisions by Student ===============
    # Multiple students booking same room at same time

    collision_tracker <- adv_stu[,
      .(StudentCount = .N),
      keyby = c("Semester", "Day", "Time", "Room")
    ][order(-StudentCount)]

    schedule[,StudentCount := NULL][collision_tracker,
      on = c("Semester", "Day", "Time", "Room"),
      StudentCount := i.StudentCount
    ][,
      StudentCount := replace_na(StudentCount, replace = 0)
    ]

    collision_tracker <- collision_tracker[StudentCount > 1]

    if (nrow(collision_tracker) > 1) {
      for (rw in seq(1, nrow(collision_tracker))) {
        student_count <- collision_tracker[rw,StudentCount]
        semester_ <- collision_tracker[rw, Semester]
        day_ <- collision_tracker[rw, Day]
        time_ <- collision_tracker[rw, Time]
        room_ <- collision_tracker[rw, Room]

        opt <- schedule[
          StudentCount == 0 &  # Keep Semester and Initial Room in place
          Semester == semester_ & Room == room_
        ]

        if (nrow(opt > 0)) {
          adv_stu[
            Semester == semester_ & Day == day_ & Time == time_ & Room == room_,
            c("Day", "Time") := rbind(
              tibble(Day = day_, Time = time_),                        # Current assignment
              opt[seq(min(nrow(opt), student_count-1)), .(Day, Time)]  # Reassigned from opt
            )
          ]
        }

        # Update schedule to keep opt accurate
        schedule[,StudentCount := NULL][adv_stu[,.(StudentCount = .N),
          keyby = c("Semester", "Day", "Time", "Room")],
          on = c("Semester", "Day", "Time", "Room"),
          StudentCount := i.StudentCount
        ][,
          StudentCount := replace_na(StudentCount, replace = 0)
        ]
      }
    }

    # Relax Room Constraint

    collision_tracker <- adv_stu[,
      .(StudentCount = .N),
      keyby = c("Semester", "Day", "Time", "Room")
    ][order(-StudentCount)]

    schedule[,StudentCount := NULL][collision_tracker,
      on = c("Semester", "Day", "Time", "Room" = "Room"),
      StudentCount := i.StudentCount
    ][,
      StudentCount := replace_na(StudentCount, replace = 0)
    ]

    collision_tracker <- collision_tracker[StudentCount > 1]

    if (nrow(collision_tracker) > 1) {
      for (rw in seq(1, nrow(collision_tracker))) {
        student_count <- collision_tracker[rw,StudentCount]
        semester_ <- collision_tracker[rw, Semester]
        day_ <- collision_tracker[rw, Day]
        time_ <- collision_tracker[rw, Time]
        room_ <- collision_tracker[rw, Room]

        opt <- schedule[
          StudentCount == 0 &
          Semester == semester_
        ]

        if (nrow(opt > 0)) {
          adv_stu[
            Semester == semester_ & Day == day_ & Time == time_ & Room == room_,
            c("Day", "Time", "Room") := rbind(
              tibble(Day = day_, Time = time_, Room = room_),                # Current assignment
              opt[seq(min(nrow(opt), student_count-1)), .(Day, Time, Room)]  # Reassigned from opt
            )
          ]
        }

        # Update schedule to keep opt accurate
        schedule[,StudentCount := NULL][adv_stu[,.(StudentCount = .N),
          keyby = c("Semester", "Day", "Time", "Room")],
          on = c("Semester", "Day", "Time", "Room"),
          StudentCount := i.StudentCount
        ][,
          StudentCount := replace_na(StudentCount, replace = 0)
        ]
      }

      collision_tracker <- adv_stu[,
        .(StudentCount = .N),
        keyby = c("Semester", "Day", "Time", "Room")
      ][order(-StudentCount)]

      schedule[,StudentCount := NULL][collision_tracker,
        on = c("Semester", "Day", "Time", "Room" = "Room"),
        StudentCount := i.StudentCount
      ][,
        StudentCount := replace_na(StudentCount, replace = 0)
      ]
    }
  }

  improve_advisor_colls <- function() {
    # ** Collisions by Advisor ===============
    # Same advisor in two rooms at same time

    collision_tracker <- adv_stu[,
      .(MeetingCount = .N),
      keyby = c("Advisor", "Semester", "Day", "Time")
    ][order(-MeetingCount)][
      MeetingCount > 1
    ]

    if (nrow(collision_tracker > 0)) {
      for (rw in seq(1, nrow(collision_tracker))) {
        meeting_count <- collision_tracker[rw,MeetingCount]
        advisor_ <- collision_tracker[rw, Advisor]
        semester_ <- collision_tracker[rw, Semester]
        day_ <- collision_tracker[rw, Day]
        time_ <- collision_tracker[rw, Time]
        room_ <- adv_stu[
          Advisor == advisor_ & Semester == semester_ & Day == day_ & Time == time_,
          Room
        ][[1]]

        opt <- schedule[StudentCount == 0]

        if (nrow(opt > 0)) {
          adv_stu[
            Advisor == advisor_ & Semester == semester_ & Day == day_ & Time == time_,
            c("Semester", "Day", "Time", "Room") := rbind(
              tibble(Semester = semester_, Day = day_, Time = time_, Room = room_),            # Current assignment
              opt[seq(min(nrow(opt), meeting_count-1)), .(Semester, Day, Time, Room)]  # Reassigned from opt
            )
          ]
        }

        # Update schedule to keep opt accurate
        schedule[,StudentCount := NULL][adv_stu[,.(StudentCount = .N),
          keyby = c("Semester", "Day", "Time", "Room")],
          on = c("Semester", "Day", "Time", "Room"),
          StudentCount := i.StudentCount
        ][,
          StudentCount := replace_na(StudentCount, replace = 0)
        ]
      }
    }
  }
  
  assign_missing_students <- function() {
    # For some reason (?), students get dropped sometimes
    # This adds them back
    unassigned <- advisor_student[!adv_stu, on = "Student"]
    if (nrow(unassigned) > 0) {
      free_spots <- schedule[StudentCount == 0][, StudentCount := NULL]
      assigned <- cbind(
        unassigned[seq(1,min(nrow(free_spots), nrow(unassigned)))],
        free_spots[seq(1,min(nrow(free_spots), nrow(unassigned)))]
      )
      adv_stu <<- rbind(adv_stu, assigned)
      
      # Update meta-data
      adv_stu[,GroupCount := .N, by = "Group"]
      schedule[,StudentCount := NULL][adv_stu[,.(StudentCount = .N),
        keyby = c("Semester", "Day", "Time", "Room")],
        on = c("Semester", "Day", "Time", "Room"),
        StudentCount := i.StudentCount
      ][,
        StudentCount := replace_na(StudentCount, replace = 0)
      ]
    }
  }

  move_free_spots <- function() {
    # This function should move free spots
    # to semesters where they are needed

    student_colls <- adv_stu[,.(
      StudentCount = .N,
      Students = sapply(list(Student), toString),
      Group = sapply(list(Group), toString)
    ),
      keyby = c("Semester", "Day", "Time", "Room")
    ][StudentCount > 1][,.N]

    advisor_colls <- adv_stu[,
      .(MeetingCount = .N),
      keyby = c("Advisor", "Semester", "Day", "Time")
    ][order(-MeetingCount)][MeetingCount > 1][,.N]

    free_spots <- schedule[StudentCount == 0][, StudentCount := NULL]

    students_to_move <- adv_stu[free_spots[,.(
      Semester = fifelse(Semester == "Fall Semester", "Spring Semester", "Fall Semester"),
      Day, Time, Room
    )], on = .(Semester, Day, Time, Room)][
      GroupCount == 1  # Not siblings
    ][
      !semester_fixed, on = "Student" # Not semester specified
    ][seq(1+student_colls+advisor_colls)]

    adv_stu[Student %in% students_to_move$Student,
      Semester := fifelse(Semester == "Fall Semester", "Spring Semester", "Fall Semester")
    ]
    
    # Update Schedule
    schedule[,StudentCount := NULL][adv_stu[,.(StudentCount = .N),
      keyby = c("Semester", "Day", "Time", "Room")],
      on = c("Semester", "Day", "Time", "Room"),
      StudentCount := i.StudentCount
    ][,
      StudentCount := replace_na(StudentCount, replace = 0)
    ]
  }
  
  sibling_cluster <- function() {
    adv_stu <<- adv_stu[order(-GroupCount, Group, Time)][,`:=`(
      Day = Day[[1]],
      Time = 
        if(Time[[1]] <= 13 - .N + 1) Time[[1]] + (seq(.N)-1) 
        else Time[[1]] - (seq(.N, 1) + 1)
    ),
      keyby = "Group"
    ]
    # Update schedule
    schedule[,StudentCount := NULL][adv_stu[,.(StudentCount = .N),
      keyby = c("Semester", "Day", "Time", "Room")],
      on = c("Semester", "Day", "Time", "Room"),
      StudentCount := i.StudentCount
    ][,
      StudentCount := replace_na(StudentCount, replace = 0)
    ]
  }
  
  initial_schedule()

  improve_student_colls()
  improve_advisor_colls()
  move_free_spots()
  sibling_cluster()
  improve_student_colls()
  improve_advisor_colls()
  move_free_spots()
  
  counter = 0
  while (counter < 25) {
    if (nrow(adv_stu) < nrow(advisor_student)) {
      move_free_spots()
      assign_missing_students()
      counter = counter + 1
    } else if (
      1 < nrow(adv_stu[,.(StudentCount = n_distinct(Student)), 
      keyby = c("Semester", "Day", "Time", "Room")
    ][StudentCount > 1])) {
      move_free_spots()
      improve_student_colls()
      counter = counter + 1
    } else if (
      1 < nrow(adv_stu[,.(MeetingCount = .N),
      keyby = c("Advisor", "Semester", "Day", "Time")
    ][MeetingCount > 1])) {
      move_free_spots()
      improve_advisor_colls()
      counter = counter + 1
    } else {
      move_free_spots()
      sibling_cluster()
      move_free_spots()
      assign_missing_students()
      move_free_spots()
      improve_student_colls()
      move_free_spots()
      improve_advisor_colls()
      counter = counter + 1
    }
  }
  
  move_free_spots()
  assign_missing_students()
  
  # Outro =======================
  
  schedule[adv_stu,
    on = c("Semester", "Day", "Time", "Room"),
    c("Student", "GradeLevel", "Advisor") := .(i.Student, i.GradeLevel, i.Advisor)
  ][,StudentCount := NULL]
  return(schedule)
}

gen_parallel <- function(i) {
  schedule <- try(schedule_gen())
  saveRDS(schedule, file = 
    paste0("./Output/", stochastic_output_folder, "/", i, ".rds" )
  )
}

library(foreach)
library(doParallel)
cl <- parallel::makeCluster(8)
registerDoParallel(cl)

foreach(
  i = seq(number_of_sims),
  .packages = c("tidyverse", "data.table"),
  .export = c("full_schedule", "time", "dt", "advisor_student")
) %dopar% gen_parallel(i)

parallel::stopCluster(cl)
