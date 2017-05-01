% Written by Mehmet Hakan Kurtoğlu, March 2017, on SWI-Prolog.

% Dynamic declarations for knowledge base manipulation
:-dynamic student/2.
:-dynamic available_slots/1.
:-dynamic room_capacity/2.

% Clears all assertions from the knowledge base and prints the number of assertions that are deleted 
clear_knowledge_base :- 
	%% Erasing all students
	findall(_,student(_,_),List), length(List,X), retractall(student(_,_)), write("student/2: "), writeln(X),
	%% Erasing available_slots
	findall(_,available_slots(_),L),length(L,A), retractall(available_slots(_)), write("available_slots/1: "), writeln(A),
	%% Erasing rooms
	findall(_,room_capacity(_,_),RList), length(RList,Y), retractall(room_capacity(_,_)), write("room_capacity/2: "), writeln(Y).

% List will contain all students in the knowledge base 
all_students(List) :- findall(X,student(X,_),List).

% CourseList will contain all students in the knowledge base
all_courses(CourseList) :- findall(X,student(_,X),List), flatten(List,TotalList), list_to_set(TotalList,CourseList).

% Gives the number of students registered to the given course 
student_count(CourseID,StudentCount):- 
	%% CourseList will contain individual lists that are courses taken by individual students, then they are all merged with flatten/2.
	findall(X,student(_,X),List), flatten(List,CourseList), 
	%% Number of times CourseID is repeated in CourseList, that is how many students registered to this course 
	count(CourseID,CourseList,0,StudentCount),!.

% Calculates how many times given item is repeated in given list
count(_,[],Numb,Numb).  %% Base case when list is empty
count(Item,[Item|T],C,Numb):- C1 is C+1, count(Item,T,C1,Numb). %% If item is in the head, count is incremented, recursion keeps on with Tail
count(Item,[Y|T],C,Numb):- Item \= Y, count(Item,T,C,Numb). %% Otherwise recursion keeps on with Tail, without any incrementing

% Gives the number of students who are enrolled in both of the given courses
common_students(CourseID1,CourseID2,StudentCount):-
	%% List is a list containing of lists, which are courses taken by individual students. Each set of courses then is inspected if it contains both courses.
	findall(X,student(_,X),List), countStudents(List,CourseID1,CourseID2,0,StudentCount),!.

% This calculates the number of students that are enrolled in both given courses, using a Tail recursive approach
countStudents([],CourseID1,CourseID2,Count,Count). %% Base case when the list is empty
countStudents([H|T],CourseID1,CourseID2,Count,StudentCount) :- 
	%% If both courses are member of the Head, count is incremented and Tail is inspected
	member(CourseID1,H), member(CourseID2,H), C1 is Count + 1, countStudents(T,CourseID1,CourseID2,C1,StudentCount)
	%% otherwise Tail gets inspected, without any incrementing
	; countStudents(T,CourseID1,CourseID2,Count,StudentCount). 


% This appoints a classroom for course, using a Tail recursive approach.
% RoomList and SlotList contains all Rooms and Slots, respectively. 
% RoomSlotPairs will contain paired RoomID's and SLots, it will be used for checking if a Room is available for given slot.
% FinalPlan will contain Finals schedule.
% At base case all courses will be appointed.
appoint_room([],RoomList,SlotList,RoomSlotPairs,FinalPlan,FinalPlan).  %% Base case
% F will be the Finals schedule being built up through the recursive procedures, at the base case FinalPlan will be unified with it
appoint_room([Course|CourseListTail],RoomList,SlotList,RoomSlotPairs,FinalPlan,F):- 
	%% Course will always be selected as the Head of the CourseList, a Slot and a Room will be selected untill no error is found
	%% is_valid/5 checks for errors
	member(Slot,SlotList), member(Room,RoomList),is_valid(Course,Room,Slot,RoomSlotPairs,F), append([Room],[Slot],Rs),
	append([Rs],RoomSlotPairs,RS2),append([Course],[Room],Pair),append(Pair,[Slot],Triple),
	%% Final schedules will be stored in Course_Room_Slot order.
	append([Triple],F,F1),appoint_room(CourseListTail,RoomList,SlotList,RS2,FinalPlan,F1). %% F1 will be updated untill the base case

% Checks if given room has enough capacity for given course
has_capacity(CourseID,RoomID):-
	student_count(CourseID,NumbStudents), room_capacity(RoomID,X), NumbStudents =< X. 

% Checks if given Course, Room and Slot all are valid i.e. no error occurs for the FinalPlan
% Errors are:
% If classroom has enough capacity for all the studnets that are taking the course,
% if classroom is free in the given slot and if there is no student that has any other final exam during the given slot. 
% This predicate is always true if no error is found when controlling above constraints.
is_valid(Course,Room,Slot,RoomSlotPairs,Plan):- has_capacity(Course,Room),no_common_student(Course,Slot,Plan),
	append([Room],[Slot],Rs), not(member(Rs,RoomSlotPairs)).

% This checks if there is a student that takes another course with a final exam in the same slot. If there is no such student, then this predicate is true.
no_common_student(Course,Slot,[]). %% Base case when all slots are checked, it's always true because if it's reached it means no errors has been found along the way.
no_common_student(Course,Slot,[H|T]):- 
	%% If the slot is different, recursion keeps on with Tail
	%% nth0/3 is built in predicate that is always true when nth0(Index,List,Item) unifies
	nth0(2,H,S1), S1 \== Slot, no_common_student(Course,Slot,T);
	%% Otherwise predicate checks if there is no student with another final exam in the same slot
	nth0(2,H,S2),S2==Slot,nth0(0,H,C1), common_students(C1,Course,StuNumb), StuNumb == 0, no_common_student(Course,Slot,T).
	
% This calculates final exam time and location for any course, without conflicts, stored in FinalPlan list
final_plan(FinalPlan):-
	%% Creating various lists for courses, rooms and slots
	all_courses(CourseList), findall(X,room_capacity(X,_),RoomList), available_slots(SlotList),
	%% Fills out FinalPlan without any errors
	appoint_room(CourseList,RoomList,SlotList,[],FinalPlan,[]).

% This checks if given FinalPlan has errors, in the case errors ErrorCount is calculated
errors_for_plan(FinalPlan,ErrorCount):- 
	%% Errors are calculated
	error_calculator(FinalPlan,ErrorCount,0).

% Calculates errors
error_calculator([],ErrorCount,ErrorCount). %% Base case when all FinalPlan is inspected.
% Count is built up along the way with each error found, at the base case it's unified with ErrorCount.
error_calculator([[Course|[Room|[Slot|_]]]|PlanTail],ErrorCount,Count):-
	%% If number of students in the room exceeds the capacity and if there exists any students that has conflicting finals in a given slot
	student_count(Course,NumbStudents),room_capacity(Room,X), ErrorForRooms is NumbStudents-X,ErrorForRooms>=0, Count1 is Count + ErrorForRooms,
	check_for_conflicts(Course,Slot,PlanTail,ErrorForSlots,0), Count2 is ErrorForSlots + Count1, error_calculator(PlanTail,ErrorCount,Count2),!;
	%% If capacity of the room exceeds number of students, then only conflicting finals are checked and added to ErrorCount.
	check_for_conflicts(Course,Slot,PlanTail,ErrorForSlots,0), Count2 is ErrorForSlots + Count, error_calculator(PlanTail,ErrorCount,Count2),!.

% Checks all other finals if there is any conflicts
check_for_conflicts(Course,Slot,[],ErrorForSlots,ErrorForSlots). %% Base case when all finals are checked
check_for_conflicts(Course,Slot,[[Course1|[Room|[Slot1|_]]]|Tail],ErrorForSlots,Count):-
	%% If there is a final in the same slot, it's checked if there is any student with conflicts
	Slot==Slot1,Course\==Course1,common_students(Course,Course1,StuNumb), Count1 is Count+StuNumb,check_for_conflicts(Course,Slot,Tail,ErrorForSlots,Count1);
	%% Otherwise other finals are checked through recursion
	Slot\==Slot1, check_for_conflicts(Course,Slot,Tail,ErrorForSlots,Count).
	
