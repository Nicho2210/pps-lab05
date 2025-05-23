package ex4

import util.Sequences.{Sequence, *}
import util.Sequences.Sequence.*

object SchoolModel:

  trait Teacher:
    def name: String
    
  object Teacher:
    def apply(name: String): Teacher = TeacherImpl(name)
    private case class TeacherImpl(override val name: String) extends Teacher

  trait Course:
    def name: String
    
  object Course:
    def apply(name: String): Course = CourseImpl(name)
    private case class CourseImpl(override val name: String) extends Course

  case class TeacherToCourse(teacher: Teacher, course: Course)

  trait School:
    def courses: Sequence[Course]
    def teachers: Sequence[Teacher]
    def teacherToCourse: Sequence[TeacherToCourse]
    extension (school: School)
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
      def hasTeacher(teacher: Teacher): Boolean
      def hasCourse(course: Course): Boolean

  object School:
    def apply(): School = BasicSchool(Sequence.Nil(), Sequence.Nil(), Sequence.Nil())

    private case class BasicSchool(private var _teachers: Sequence[Teacher],
                                   private var _courses: Sequence[Course],
                                   private var _teacherToCourse: Sequence[TeacherToCourse]) extends School:
      override def teachers: Sequence[Teacher] = _teachers
      override def courses: Sequence[Course] = _courses
      override def teacherToCourse: Sequence[TeacherToCourse] = _teacherToCourse

      extension (school: School)
        def setTeacherToCourse(teacher: Teacher, course: Course): School = {
          if !_teachers.contains(teacher) then _teachers = _teachers.concat(Cons(teacher, Nil()))
          if !_courses.contains(course) then _courses = _courses.concat(Cons(course, Nil()))
          _teacherToCourse = _teacherToCourse.concat(Cons(TeacherToCourse(teacher, course), Nil()))
          this
        }

        def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
          _teacherToCourse.filter(_.teacher == teacher).map(_.course)

        def hasTeacher(teacher: Teacher): Boolean = _teachers.contains(teacher)

        def hasCourse(course: Course): Boolean = _courses.contains(course)

@main def examples(): Unit =
  import SchoolModel.*
  val school = School()
  import school.*
  println(school.teachers) // nil
  println(school.courses) // nil
  println(school.hasTeacher(Teacher("John"))) // false
  println(school.hasCourse(Course("Math"))) // false
  val john = Teacher("John")
  val math = Course("Math")
  val italian = Course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // John :: nil
  println(school2.courses) // Math :: nil
  println(school2.hasTeacher(john)) // true
  println(school2.hasCourse(math)) // true
  println(school2.hasCourse(italian)) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers) // John :: nil
  println(school3.courses) // Math :: Italian :: nil
  println(school3.hasTeacher(john)) // true
  println(school3.hasCourse(math)) // true
  println(school3.hasCourse(italian)) // true
  println(school3.coursesOfATeacher(john)) // Math :: Italian :: nil