library(tidyverse)
library(data.table)

run_date <- "10022023"
stochastic_output_folder <- "2023-2024_sims"

# Import Data (see Conferences/R/data_extract.R)

dt <- readRDS(file = paste0("./data/conference_", run_date, ".rds")) %>% 
  data.table(., 
    key = c("GradeLevel", "Student", "Advisor", "Duration", "Teacher")
  )

export_folder <- "./Check This Out v2/"

# Provide "Parent" instead of "Group
stu_par <- readxl::read_excel(
  path = paste0("./Source/Student_Parent_", run_date, ".xlsx"),
)
stu_par <- data.table(stu_par)
stu_par <- stu_par[,
  .SD[1], 
  keyby = c("Student Lastname", "Student Firstname")
][,
  Group := .GRP,
  by = c("Parent Lastname", "Parent Firstname")
][,
  Parent := paste(`Parent Lastname`, `Parent Firstname`, Email)
]

dt[stu_par, on = "Group", Parent := i.Parent]


export_schedule <- function(i) {
  schedule <- readRDS(
    file = paste0("./data/", stochastic_output_folder, "/", i, ".rds" )
  )
  
  conf_data <- dt[schedule,
    on = .(Advisor, Student),
    `:=` (
      Semester = i.Semester,
      Day = i.Day,
      Time = i.Time,
      Room = i.Room
    )][
      Duration == Semester
    ]
  
  # Teacher Goodness =================
  
  # Teachers by meeting
  teacher_schedule <- conf_data[,.(
    MeetingCount = n_distinct(Student),
    Students = sapply(list(Student), toString),
    Rooms = sapply(list(Room), toString)
  ),
    keyby = c("Teacher", "Semester", "Day", "Time")
  ]
  
  advisor_schedule <- conf_data[,.(
    MeetingCount = n_distinct(Student),
    Students = sapply(list(unique(Student)), toString),
    Rooms = sapply(list(unique(Room)), toString)
  ),
    keyby = c("Advisor", "Semester", "Day", "Time")
  ]
  
  teacher_aux_data <- conf_data[,
    TeacherMeetingCount := n_distinct(Student),
    by = c("Teacher", "Semester", "Day", "Time")
  ][,.(
    Teachers = sapply(list(sort(unique(Teacher))), toString)),
    keyby = c("Semester", "Day", "Time", "Room", "TeacherMeetingCount")
  ] %>% 
    pivot_wider(
      names_from = TeacherMeetingCount, 
      values_from = Teachers,
      names_prefix = "Teacher_"
    )
  
  full_schedule <- full_join(
    x = schedule,
    y = teacher_aux_data,
    by = c("Semester", "Day", "Time", "Room")
  ) %>% 
    left_join(
      x = .,
      y = dt %>% select(Student, Parent) %>% distinct(Student, .keep_all = TRUE),
      by = "Student"
    )
  
  parent_schedule <- full_schedule %>% 
    arrange(Parent) %>% 
    group_by(Parent) %>% mutate(MeetingCount = n()) %>% 
    select(Parent, MeetingCount, Student, GradeLevel, Semester, Day, Time, Room)
  
  list(
    FullSchedule = full_schedule,
    TeacherSchedule = teacher_schedule,
    AdvisorSchedule = advisor_schedule,
    ParentSchedule = parent_schedule
  ) %>% 
    writexl::write_xlsx(x = ., path = paste0(export_folder, "schedule_", i, ".xlsx"))
}

export_schedule(1058)
