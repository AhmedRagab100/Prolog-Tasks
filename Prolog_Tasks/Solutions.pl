:-include('students_courses.pl').

%
memberTest(El, [H|T]) :-
      member_(T, El, H).
  member_(_, El, El).
  member_([H|T], El, _) :-
      member_(T, El, H).
%
addElement([], L, L).
addElement([H|T], L, [H|R]) :-
  addElement(T, L, R).
%-------Task1--------

getStudents(Sub,List):-
    getStudents(Sub,[],List).

getStudents(Sub,Temp,List):-
    student(X,Sub,Z),
    not(memberTest([X,Z],Temp)),

    addElement([[X,Z]],Temp,New),

    getStudents(Sub,New,List),!.

getStudents(Sub,List,List).
%-----------------Task2--------------

len([], LenResult):-
    LenResult is 0.
len([X|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.

numStudents(Course,Num):-
   numStudents(Course,[],List),
   len(List,Num),!.

numStudents(Course,Tmplist,List):-
   student(X,_,_),student(X,Course,_),
   not(memberTest(X,Tmplist)), !,
   addElement([X],Tmplist,NewTmplist),
   numStudents(Course,NewTmplist,List).

numStudents(_,List,List).

%-----Task3------

getMax([Max],Max).
getMax([H1,H2|T],Max):-
    H1 > H2,
    getMax([H1|T],Max),!.

getMax([H1,H2|T],Max):-
    H1 < H2,
    getMax([H2|T],Max),!.

getMaxGrades(Stud,Max_Grades):-
    getGrades(Stud,[],Grades),
    getMax(Grades,Max_Grades).

getGrades(Stud,Tmplist,Grades):-
    student(Stud,Y,Z),
    not(memberTest(Z,Tmplist)),

    addElement([Z],Tmplist,Newlist),
    getGrades(Stud,Newlist,Grades),!.

getGrades(Stud,Grades,Grades).

%---------------Task4---------------

getGrade(X,Course,Gradeinword):-
  student(X,Course,Grade),grade_to_words(Grade,Gradeinword),!.

grade_to_words(0, []).
grade_to_words(Number, Wordlist) :-
    Number > 0,
    RemainingNumber is div(Number, 10),
    LastDigit is mod(Number, 10),
    numToWord(LastDigit,Word),
    addElement(RemainingDigits,[Word], Wordlist),
    grade_to_words(RemainingNumber, RemainingDigits),!.

numToWord(Num,Word):-
    Num is 0,Word = zero;
    Num is 1,Word = one;
    Num is 2,Word = two;
    Num is 3,Word = three;
    Num is 4,Word = four;
    Num is 5,Word = five;
    Num is 6,Word = six;
    Num is 7,Word = seven;
    Num is 8,Word = eight;
    Num is 9,Word = nine.
%---------------Task5---------------

getprecourses(Course,List,Result):-
	prerequisite(Pre,Course),
        addElement([Pre],List,NewList),
	getprecourses(Pre,NewList,Result),!.

getprecourses(_,Target,Target).

getonefromlist([Head|Rest],Head,Rest).

remainingCourses(Stud,Course,PreCourses):-
	getprecourses(Course,[],Target),
	getonefromlist(Target,Head,Rest),
	student(Stud,Head,StudGrade),StudGrade>50,
	PreCourses=Rest,!.
%
