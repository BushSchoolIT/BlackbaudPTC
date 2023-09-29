library(tidyverse)
library(data.table)

run_date <- "9132022"
semester1 <- "fall_2022"
semester2 <- "spring_2023"

# Student Parent ==================

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

# Course Enrollment ==================

ce1 <- readxl::read_excel(
  path = paste0("./Source/course_enrollments_", semester1, "_", run_date, ".xlsx")
) %>% 
  separate_rows( `Student List`, sep = ",") %>% 
  mutate(
    Student = trimws(`Student List`)
  )

ce2 <- readxl::read_excel(
  path = paste0("./Source/course_enrollments_", semester2, "_", run_date, ".xlsx")
) %>% 
  separate_rows( `Student List`, sep = ",") %>% 
  mutate(
    Student = trimws(`Student List`)
  )


# Advisor by Grade ==================

abg <- readxl::read_excel(
  path = paste0("./Source/advisor_by_grade_", run_date, ".xlsx")
)

colnames(abg) <- c("Student", "Advisor", "Advisory")

abg <- abg %>% 
  mutate(
    GradeLevel = if_else(is.na(Advisor), Student, NA_character_)
  ) %>% 
  fill(GradeLevel, .direction = "down") %>% 
  filter(Student != "Student" & !is.na(Advisor)) %>% 
  # split off graduation year
  separate(col = Student, into = c("Student", "GradYear"), sep = -3) %>% 
  mutate(
    GradYear = substr(GradYear, 2, 3),
    Student = trimws(Student),
    GradYear = as.integer(2000L + as.numeric(GradYear))
  ) %>% 
  # Split off last name
  separate(col = Student, into = c("LastName", "OtherName"), sep = ",") %>% 
  mutate(
    LastName = trimws(LastName),
    OtherName = trimws(OtherName)
  ) %>% 
  # Split off preferred name
  separate(col = OtherName, into = c("FirstName", "PreferredName"), sep = "[(]") %>% 
  mutate(
    FirstName = trimws(FirstName)
  ) %>% 
  select(-PreferredName) %>% 
  mutate(
    JoinName = paste0(FirstName, " ", LastName)
  )

abg_ce_1 <- full_join(
  x = abg %>% 
    select(JoinName, GradeLevel, GradYear, Advisor, Advisory),
  y = ce1 %>% 
    select(Student, Teacher, Section, Block, Duration),
  by = c("JoinName" = "Student")
) %>% 
  rename(Student = JoinName) %>% 
  # For GradeLevel sorting
  mutate(
    GradeLevel = factor(GradeLevel, 
      levels = c("9th Grade", "10th Grade", "11th Grade", "12th Grade"),
      ordered = TRUE
    )
  ) %>% 
  arrange(GradeLevel, Student)

abg_ce_2 <- full_join(
  x = abg %>% 
    select(JoinName, GradeLevel, GradYear, Advisor, Advisory),
  y = ce2 %>% 
    select(Student, Teacher, Section, Block, Duration),
  by = c("JoinName" = "Student")
) %>% 
  rename(Student = JoinName) %>% 
  # For GradeLevel sorting
  mutate(
    GradeLevel = factor(GradeLevel, 
      levels = c("9th Grade", "10th Grade", "11th Grade", "12th Grade"),
      ordered = TRUE
    )
  ) %>% 
  arrange(GradeLevel, Student)

output <- bind_rows(abg_ce_1, abg_ce_2) %>% 
  left_join(
    x = .,
    y = stu_par %>% 
      mutate(Student = paste(`Student Firstname`, `Student Lastname`)) %>% 
      select(Student, Group) %>% 
      distinct(),
    by = "Student"
  ) %>% 
  arrange(Group, GradeLevel, Student, Duration)

saveRDS(output, file = paste0("./data/conference_", run_date, ".rds"))
writexl::write_xlsx(output, path = paste0("./data/conference_", run_date, ".xlsx"))

