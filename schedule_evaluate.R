library(tidyverse)
library(data.table)

run_date <- "9132022"
stochastic_output_folder <- "2022-2023 Sims v2"
number_of_sims <- 30000

# Import Data (see Conferences/R/data_extract.R)

dt <- readRDS(file = paste0("./Output/conference_", run_date, ".rds")) %>% 
  data.table(., 
    key = c("GradeLevel", "Student", "Advisor", "Duration", "Teacher")
  )

students_to_schedule <- unique(dt[GradeLevel != "12th Grade", Student])

semester_fixed <- data.table(
  Student = c("Ava Armbruster", "Cole Masters", "Eve Hinderliter", "Geoffrey Arone", 
              "Lauren Eva", "Maile Krueger", "Makie Koizumi-Hachey"),
  Semester = rep("Fall Semester", times = 7),
  key = "Student"
)

schedule_eval <- function(i) {
  
  schedule <- readRDS(
    file = paste0("./Output/", stochastic_output_folder, "/", i, ".rds" )
  )
  
  # Tests for consistency & completeness ==================
  # at most 1 student per time & place
  cons_student <- schedule[,.(
    StudentCount = n_distinct(Student),
    Students = sapply(list(Student), toString)
  ),
    by = c("Semester", "Day", "Time", "Room")
  ][StudentCount > 1][,StudentCount := NULL]
  
  # All students are scheduled
  missing_students <- setdiff(
    students_to_schedule, schedule[!is.na(Student),unique(Student)]
  )
  
  # at most 1 advisor per time
  cons_advisor <- schedule[,.(
    PlaceCount = n_distinct(Room),
    Rooms = sapply(list(Room), toString)
  ),
    by = c("Advisor", "Semester", "Day", "Time")
  ][PlaceCount > 1][,PlaceCount := NULL]
  
  # advisors are present
  students_missing_advisors <- schedule[!is.na(Student) & is.na(Advisor), Student]
  
  # fixed_semester students correctly assigned
  semester_assigned_errors <- left_join(
    x = semester_fixed,
    y = schedule %>% select(Student, Semester),
    by = "Student",
    suffix = c("_fixed", "_assigned")
  ) %>% filter(Semester_fixed != Semester_assigned) %>% 
    summarize(
      StudentSemesterError = sapply(list(Student), toString)
    )
  
  open_slots <- schedule[is.na(Student)]
  
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
  teacher_attendance_stats <- teacher_schedule[,.(
    StudentCount = sum(MeetingCount),
    Meetings = .N
  ),
    by = c("Teacher")
  ][,
    AttendancePct := Meetings/StudentCount
  ][,.(
    TA_LowestPct = min(AttendancePct),
    TA_AvgPct = mean(AttendancePct)
  )]
  
  #  fewer back-to-back in a day
  teacher_back2back_stats <- teacher_schedule[,`:=`(
    Prior1 = 1*(lag(Time, n = 1L, default = -Inf) == Time - 1L),
    Prior2 = 1*(lag(Time, n = 2L, default = -Inf) == Time - 2L),
    Prior3 = 1*(lag(Time, n = 3L, default = -Inf) == Time - 3L),
    Prior4 = 1*(lag(Time, n = 4L, default = -Inf) == Time - 4L),
    Prior5 = 1*(lag(Time, n = 5L, default = -Inf) == Time - 5L),
    Prior6 = 1*(lag(Time, n = 6L, default = -Inf) == Time - 6L),
    Prior7 = 1*(lag(Time, n = 7L, default = -Inf) == Time - 7L),
    Prior8 = 1*(lag(Time, n = 8L, default = -Inf) == Time - 8L),
    Prior9 = 1*(lag(Time, n = 9L, default = -Inf) == Time - 9L),
    Prior10 = 1*(lag(Time, n = 10L, default = -Inf) == Time - 10L),
    Prior11 = 1*(lag(Time, n = 11L, default = -Inf) == Time - 11L),
    Prior12 = 1*(lag(Time, n = 12L, default = -Inf) == Time - 12L)
  ),
    by = c("Teacher", "Semester", "Day")
  ][,.(
    AvgPrior1 = mean(Prior1),
    AvgPrior2 = mean(Prior1 * Prior2),
    AvgPrior3 = mean(Prior1 * Prior2 * Prior3),
    AvgPrior4 = mean(Prior1 * Prior2 * Prior3 * Prior4),
    AvgPrior5 = mean(Prior1 * Prior2 * Prior3 * Prior4 * Prior5),
    AvgPrior6 = mean(Prior1 * Prior2 * Prior3 * Prior4 * Prior5 * Prior6),
    AvgPrior7 = mean(Prior1 * Prior2 * Prior3 * Prior4 * Prior5 * Prior6 * Prior7),
    AvgPrior8 = mean(Prior1 * Prior2 * Prior3 * Prior4 * Prior5 * Prior6 * Prior7 * Prior8),
    AvgPrior9 = mean(Prior1 * Prior2 * Prior3 * Prior4 * Prior5 * Prior6 * Prior7 * Prior8 * Prior9)
  ),
    by = c("Teacher")
  ][,.(
    T1_mean = mean(AvgPrior1),
    T1_max = max(AvgPrior1),
    T2_mean = mean(AvgPrior2),
    T2_max = max(AvgPrior2),
    T3_mean = mean(AvgPrior3),
    T3_max = max(AvgPrior3),
    T4_mean = mean(AvgPrior4),
    T4_max = max(AvgPrior4),
    T5_mean = mean(AvgPrior5),
    T5_max = max(AvgPrior5),
    T6_mean = mean(AvgPrior6),
    T6_max = max(AvgPrior6),
    T7_mean = mean(AvgPrior7),
    T7_max = max(AvgPrior7),
    T8_mean = mean(AvgPrior8),
    T8_max = max(AvgPrior8),
    T9_mean = mean(AvgPrior9),
    T9_max = max(AvgPrior9)
  )]
  
  # Teacher Goodness
  #  balance between days
  teacher_balance_stats <- teacher_schedule[,.(
    MeetingCount = .N
  ),
    by = c("Teacher", "Semester", "Day")
  ][,
    MeetingPct := MeetingCount/sum(MeetingCount),
    by = c("Teacher", "Semester")
  ][,.(
    # 0 means uniform distribution, 1 = all meetings on one day
    ScaledKLDivergence = 1/log(3)*sum(MeetingPct*log(3*MeetingPct)) # 3 for 3 days, assumes uniform distribution
  ),
    by = c("Teacher", "Semester")
  ][,.(
    Teacher_Balance_mean = mean(ScaledKLDivergence),
    Teacher_Balance_max = max(ScaledKLDivergence)
  )]
  
  # Advisor goodness ====================
  
  advisor_schedule <- conf_data[,.(
    MeetingCount = n_distinct(Student),
    Students = sapply(list(unique(Student)), toString),
    Rooms = sapply(list(unique(Room)), toString)
  ),
    keyby = c("Advisor", "Semester", "Day", "Time")
  ]
  
  # Advisor goodness
  #  fewer back-to-back in a day
  advisor_back2back_stats <- advisor_schedule[,`:=`(
    Prior1 = 1*(lag(Time, n = 1L, default = -Inf) == Time - 1L),
    Prior2 = 1*(lag(Time, n = 2L, default = -Inf) == Time - 2L),
    Prior3 = 1*(lag(Time, n = 3L, default = -Inf) == Time - 3L),
    Prior4 = 1*(lag(Time, n = 4L, default = -Inf) == Time - 4L),
    Prior5 = 1*(lag(Time, n = 5L, default = -Inf) == Time - 5L),
    Prior6 = 1*(lag(Time, n = 6L, default = -Inf) == Time - 6L)
  ),
    by = c("Advisor", "Semester", "Day")
  ][,.(
    AvgPrior1 = mean(Prior1),
    AvgPrior2 = mean(Prior1 * Prior2),
    AvgPrior3 = mean(Prior1 * Prior2 * Prior3),
    AvgPrior4 = mean(Prior1 * Prior2 * Prior3 * Prior4),
    AvgPrior5 = mean(Prior1 * Prior2 * Prior3 * Prior4 * Prior5),
    AvgPrior6 = mean(Prior1 * Prior2 * Prior3 * Prior4 * Prior5 * Prior6)
  ),
  by = c("Advisor")
  ][,.(
    A1_mean = mean(AvgPrior1),
    A1_max = max(AvgPrior1),
    A2_mean = mean(AvgPrior2),
    A2_max = max(AvgPrior2),
    A3_mean = mean(AvgPrior3),
    A3_max = max(AvgPrior3),
    A4_mean = mean(AvgPrior4),
    A4_max = max(AvgPrior4),
    A5_mean = mean(AvgPrior5),
    A5_max = max(AvgPrior5),
    A6_mean = mean(AvgPrior6),
    A6_max = max(AvgPrior6)
  )]
  
  # Advisor Goodness
  #  balance between days
  advisor_balance_stats <- advisor_schedule[,.(
    MeetingCount = .N
  ),
    by = c("Advisor", "Semester", "Day")
  ][,
    MeetingPct := MeetingCount/sum(MeetingCount),
    by = c("Advisor", "Semester")
  ][,.(
    # 0 means uniform distribution, 1 = all meetings on one day
    ScaledKLDivergence = 1/log(3)*sum(MeetingPct*log(3*MeetingPct)) # 3 for 3 days, assumes uniform distribution
  ),
    by = c("Advisor", "Semester")
  ][,.(
    Advisor_Balance_mean = mean(ScaledKLDivergence),
    Advisor_Balance_max = max(ScaledKLDivergence)
  )]
  
  # Student Goodness ====================
  # Similar to teacher attendance but measured from
  # average student point-of-view rather than average teacher
  student_attendance <- conf_data[,.(
    Student, GradeLevel, Teacher, Semester, Day, Time
  )][teacher_schedule,
    on = c("Teacher", "Semester", "Day", "Time"),
    PartialAttendance := 1/i.MeetingCount
  ][,.(
    AttendancePct = sum(PartialAttendance)/.N
  ),
    by = "Student"
  ][,.(
    TA_lowest_student = min(AttendancePct),
    TA_avg_student = mean(AttendancePct)
  )]
  
  # Split by Semester/Grade
  semester_grade_balance <- schedule[!is.na(Student),
    .N, 
    by = c("Semester", "GradeLevel")
  ][,
    Pct:=N/sum(N)
  ][,.(
    Semester_Grade_Balance = 1/log(6)*sum(Pct*log(6*Pct))
  )]
  
  # Parent Goodness ====================
  # Back-to-back for parents
  
  parent_schedule <- conf_data[,.(
    MeetingCount = n_distinct(Student),
    Students = sapply(list(unique(Student)), toString),
    Rooms = sapply(list(unique(Room)), toString)
  ),
    keyby = c("Group", "Semester", "Day", "Time")
  ]
  
  # parent groups that are not sequential
  parent_problems <- parent_schedule[,.(
    Days = n_distinct(Day),
    Time_Range = max(Time)-min(Time),
    StudentCount = n_distinct(Students)
  ),
    by = "Group"
  ][,`:=`(
    Back2Back = fifelse(Days == 1 & StudentCount == Time_Range + 1, 1, 0)
  )][Back2Back == 0,.(BadGroups = sapply(list(unique(Group)), toString))]
  
  
  # Export ==============================
  cbind(
    data.frame(Sim = i),
    data.frame(StudentConsError = nrow(cons_student)),
    data.frame(MissingStudents = length(missing_students)),
    data.frame(AdvisorConsError = nrow(cons_advisor)),
    data.frame(MissingAdvisors = length(students_missing_advisors)),
    semester_assigned_errors,
    data.frame(ParentConsError = nrow(parent_schedule[MeetingCount > 1])),
    parent_problems,
    data.frame(OpenSlots = nrow(open_slots)),
    semester_grade_balance,
    student_attendance, 
    teacher_attendance_stats,
    teacher_back2back_stats,
    teacher_balance_stats,
    advisor_back2back_stats, 
    advisor_balance_stats
  )
}

library(foreach)
library(doParallel)
cl <- parallel::makeCluster(8)
registerDoParallel(cl)

stats <- foreach(
  i = seq(number_of_sims),
  .packages = c("tidyverse", "data.table"),
  .export = c("dt", "students_to_schedule"),
  .errorhandling = "remove",
  .combine = bind_rows,
  .inorder = FALSE,
  .multicombine = TRUE
) %dopar% schedule_eval(i)

parallel::stopCluster(cl)

writexl::write_xlsx(
  x = stats,
  path = paste0("./Output/", stochastic_output_folder, "/eval_stats_", run_date, ".xlsx")
)



