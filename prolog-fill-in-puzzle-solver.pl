% Author Name: Patricia Widjojo <pwidjojo@student.unimelb.edu.au>
% Author Student Id: 913557

% A Fill-in Puzzle solver given Puzzle and Word List coded in Prolog

% Detailed Summary: 
% A solver for fill-in puzzle that takes in a Puzzle represented as
% a list of list along with a WordList of words the puzzle should be filled 
% with. The solver will return a filled in puzzle if all the words from 
% wordlist can be fitted in the puzzle appropriately or false if not possible.
% Completed as part of COMP90048 2023 Sem 1 Project 1 Submission.

% Approach:
% Basically, each empty unfilled square in the input Puzzle is given
% a logical variable equivalent (done automatically by Prolog). The horizontal
% and vertical slots (i.e. more than 2 non '#' contiguous squares) are saved
% in either their logical variable format, or if filled, their letters. 
% For each slot, the possible words from WordList that can fill it considering 
% word length and pre-filled letters are then also saved together. These are 
% then saved in a list of list called Options in the format:
% [Slot1, [Possible words for Slot1], ... , SlotX, [Possible words for SlotX]].
% The Options are then sorted by amount of word options each slot has, from
% the least to the most. The best slot to fill (i.e. least possible word choice) 
% will then be bound to a word from the possible words. A choicepoint will 
% be left for the other possible words for the best slot filled. Then filled
% in slot along with one occurence of the word from WordList are removed from
% The slot and word list respectively. The entire process of creating and
% sorting options, trying to bind a word etc. is repeated/backtracked to until
% Prolog completely fills the Puzzle with the word from WordList or no solution
% is found and false is returned. A further breakdown of the steps involved
% in the approach is labelled on top of each predicate below.

% Libraries required: 
% Clpdf library transpose will be used to get vertical slots.
:-ensure_loaded(library(clpfd)).

% Code to Solve Fill-in Puzzle.
% Step 0.
% - Main function call, controls the program. 
% Basically gets slot, then starts filling puzzle. Returns the filled up 
% puzzle if possible or false if it is not possible to fill the puzzle 
% with the given wordlist.
puzzle_solution(Puzzle, WordList):-
    get_all_slots(Puzzle,AllSlots),
    fill_puzzle(AllSlots, WordList).

% Step 1. 
% - Get all available slots from puzzle and returns it as a list of list.
% Available slots in this case means both horizontal and vertical slots. 
% A slot is defined as contiguous elements of length > 2 in the puzzle 
% that are made up of either _ or lower case letters. This function gets all 
% horizontal slots, then transponses the puzzle and adds all vertical slots.
get_all_slots(Puzzle, AllSlots):-
    get_hv_slots(Puzzle, [], HSlots),
    transpose(Puzzle, VPuzzle),
    get_hv_slots(VPuzzle, HSlots, HVSlots),
    AllSlots = HVSlots.

% 1.1. Gets the slots from one orientation, processes row per row by
% calling get_hv_slots_row to get the slots in each row and adds it together.
get_hv_slots(Puzzle, AllSlots, AddedHVSlots):- 
    get_hv_slots(Puzzle, AllSlots, AddedHVSlots, AllSlots).

get_hv_slots(Puzzle, AllSlots, AddedHVSlots, Accum):-
    (Puzzle == [] -> AddedHVSlots = Accum ;
    Puzzle = [Row|Rest],
        get_hv_slots_row(Row, Accum, AddedRowSlots),
        Accum1 = AddedRowSlots,
        get_hv_slots(Rest, AllSlots, AddedHVSlots, Accum1)).

% 1.1.1. Gets the slots from one row. Goes left to right and 
% leverages two accumulator, one accumulator to collate all slots 
% found so far in the row and another current accumulator whenever a 
% non '#' character is found, if this ends up being a valid slot 
% (i.e. length > 2), will append to the slot accumulator.
get_hv_slots_row(PuzzleRow, AllSlots, AddedRowSlots):- 
    get_hv_slots_row(PuzzleRow, AllSlots, AddedRowSlots, [], AllSlots).

get_hv_slots_row(PuzzleRow, AllSlots, AddedRowSlots, AccCurr, AccSlots):-
    (PuzzleRow == [] -> 
    % Once it hits the end of row, check if what is in the current 
    % accumulator is valid, if yes append to slot accumulator and return,
    % if not then return slot accumulator as is.
    (length(AccCurr, Len), Len >= 2 -> AddedRowSlots = [AccCurr|AccSlots];
    AddedRowSlots = AccSlots);
        PuzzleRow = [E|Rest],
        (E \== '#' ->
        length(AccCurr, Len),
            LenPlusOne is Len + 1,
            length(AccCurr1, LenPlusOne),
            append(AccCurr,[E], AccCurr1),
            AccSlots1 = AccSlots;
        (length(AccCurr, Len), Len >= 2 -> 
            AccSlots1 = [AccCurr|AccSlots];
            AccSlots1 = AccSlots),
            AccCurr1 = []),
        get_hv_slots_row(Rest, AllSlots, AddedRowSlots, AccCurr1, AccSlots1)).

% Step 2. 
% - Main solver. Fills puzzle from the slot with least possible word options.
% Takes in all current valid slots, checks if any of the slots are fully filled. 
% If there are any, remove this filled slot and remove the corresponding 
% word from the wordlist. Then, get an option list with words that can validly 
% fill each slot considering length and pre-filled letters. Options are then
% sorted by those with the least number of possible words that can fill it.
% Then best slot to fill (i.e. least possible word choice) will then be bound 
% to a word from the possible words. A choicepoint will be left for the other
% possible words for the best slot filled. Repeat process from checking if 
% slots are filled (newly bound slot will now be found as filled and 
% consequently removed) until all slots are filled or no solutions found.
fill_puzzle(AllSlots, WordList):-
    remove_filled_words(AllSlots, WordList, AllSlotsLeft, WordListLeft),
    (AllSlotsLeft = [] -> !;
    get_all_options_list(AllSlotsLeft, WordListLeft, AllOptions),
        sort_options_list(AllOptions, SortedAllOptions),
        bind_best_option(SortedAllOptions),
        fill_puzzle(AllSlotsLeft, WordListLeft)).

% 2.1 Checks for slots that are fully filled with letters. These slots and the 
% word they are filled with then removed from thei corresponding lists.
remove_filled_words(AllSlots, WordList, AllSlotsLeft, WordListLeft):-
    check_filled(AllSlots, AllSlotsLeft, WordsDone),
    remove_words_done(WordList, WordsDone, WordListLeft).

% 2.1.1 Removes slots that are filled and gets the words it is filled with.
check_filled([Slot|Rest], AllSlotsLeft, WordsDone):- 
    check_filled([Slot|Rest], AllSlotsLeft, WordsDone, [], []).

check_filled([],AllSlotsLeft,WordsDone,AllSlotsLeft,WordsDone).
check_filled([Slot|Rest], AllSlotsLeft, WordsDone, AccumSlots, AccumWords):-
    (ground(Slot) -> 
    AccumSlots1 = AccumSlots,
        AccumWords1 = [Slot|AccumWords] ;
    AccumSlots1 = [Slot|AccumSlots],
        AccumWords1 = AccumWords),
    check_filled(Rest, AllSlotsLeft, WordsDone, AccumSlots1, AccumWords1).

% 2.1.2. Remove the first occurence of words found to be filled by 
% check_filled/3 from wordlist. 
remove_words_done(WordListLeft,[],WordListLeft).
remove_words_done(WordList, [FilledWord|Rest], WordListLeft):-
    remove_word(WordList, FilledWord, WordList1),
    remove_words_done(WordList1, Rest, WordListLeft).

%2.1.2.1. Remove first occurence of word in WordList.
remove_word(WordList, FilledWord, WordList1):- 
    member(FilledWord, WordList),
    remove_word(WordList, FilledWord, WordList1, []).

remove_word([],_,[],_).
remove_word([Word|Rest], FilledWord, WordList1, AccumList):- 
    (Word == FilledWord -> append(AccumList,Rest,WordList1);
    AccumList1 = [Word|AccumList],
    remove_word(Rest, FilledWord, WordList1, AccumList1)).

% 2.2. For each slot, get all the words from current wordlist that can validly
% fill it. This takes into account letters within slots that are filled already
% for example, ['_',b,'_'] would have ['a','b','c'] but not ['a','p','c']. The 
% option list are saved and returned in the format:
% [Slot1, [Possible words for Slot1], ... , SlotX, [Possible words for SlotX]]
get_all_options_list(AllSlots, WordList, AllOptions):- 
    get_all_options_list(AllSlots, WordList, AllOptions, []).

get_all_options_list([],_,AllOptions,AllOptions).
get_all_options_list([Slot|Rest], WordList, AllOptions, Accum):-
    get_one_slot_options(Slot, WordList, Option),
    Accum1 = [[Slot, Option]|Accum],
    get_all_options_list(Rest, WordList, AllOptions, Accum1).

% 2.2.1. Get all the possible words from wordlist that can fill one slot taking
% into account letters already in slot for partially filled slots.
get_one_slot_options(Slot, WordList, Option):- 
    get_one_slot_options(Slot, WordList, Option, []).

get_one_slot_options(Slot, WordList, Option, Accum):-
    (WordList = [] -> Option = Accum;
    WordList = [Word|Rest],
        (length(Slot, Len), length(Word, Len), is_word_fit(Slot, Word) 
        -> Accum1 = [Word|Accum];
        Accum1 = Accum),
        get_one_slot_options(Slot, Rest, Option, Accum1)).

% 2.2.1.1. Checks if a word fits a slott i.e. if it can be bound to it.
is_word_fit([],[]).
is_word_fit([CharS|RestS], [CharW|RestW]):-
    (var(CharS) -> is_word_fit(RestS, RestW);
    (CharS \== CharW -> false; is_word_fit(RestS,RestW))).

% 2.3 Sort the option list starting from the slot with the least amount of
% options (i.e. least amount of words from current wordlist that it can be 
% filled with.
sort_options_list(AllOptions,SortedAllOptions):- 
  maplist(sort_options_add_len,AllOptions,AllOptions1), 
  sort(0,@=<,AllOptions1, AllOptions2),
  maplist(sort_options_del_len,AllOptions2,SortedAllOptions).

% 2.3.1. Helper functions in sorting options list. Creates a temporary field
% of length representing the number of words slot can be filled with.
sort_options_add_len([Slot,List],[Len,Slot,List]):- length(List,Len).
sort_options_del_len([_,Slot,List],[Slot,List]).

% 2.4. Try binding the slot with least word options. Leaves choicepoint such 
% that other valid word options for the same slot can be tried if the first 
% one fails in other stages (i.e. resulted in some slot being unable to be 
% filled).
bind_best_option(SortedAllOptions):-
    SortedAllOptions = [Option|_],
    Option = [Slot,WordOption|_],
    length(WordOption, Len),
	Len > 0,
    member(Word, WordOption),
    Word = Slot.
