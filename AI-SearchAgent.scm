;Siva Sivasubramoniam Jayaram


;Main function to execute ID-DFS
;(id-dfs '(Tennessee Iowa Kentucky North-Carolina Missouri))
;((North-Carolina Tennessee Kentucky Missouri Iowa) ((1 2) (1 4) (4 5)))
;(id-dfs '(Wildcard Oregon Arizona))
;(((Oregon (Wildcard Nevada) Arizona) ((1 2))) ((Oregon (Wildcard California) Arizona) ((1 2))))
(define (id-dfs inputState)
   (if (null? inputState)
	(list inputState '())
	(let 
		((head (car inputState)) (initFrontierList (list (list inputState '()))))
		(if (equal? head 'Wildcard)
			(if (null? (cdr inputState))
				#f
				(if (is-Components-Connected-Wildcard? (cdr inputState))        ;#t ==> Call dfs and return result, #f -> Components are not connected
					(dfsHelper initFrontierList 0 (- (getLengthOfTheInputList (car (car initFrontierList))) 1) #t) 
					#f
				)
			)
			(if (is-Components-Connected? inputState)          	 	            ;#t ==> Call dfs and return result, #f -> Components are not connected
				(dfsHelper initFrontierList 0 (- (getLengthOfTheInputList (car (car initFrontierList))) 1) #f) 
				#f
			)
		)
	) 
   )
)


;Main function to execute A*
;(A* '(Tennessee Iowa Kentucky North-Carolina Illinois Indiana Wisconsin Ohio Michigan Virginia Wisconsin Minnesota Kansas Nebraska Oklahoma Arkansas Missouri Texas)) 
;((Tennessee North-Carolina Virginia Kentucky Illinois Indiana Ohio Michigan Wisconsin Minnesota Wisconsin Iowa Nebraska Kansas Missouri Arkansas Oklahoma Texas) ((2 10) (7 9) (15 17) (7 8) (3 4) (2 3) (10 12) (13 14)))
;(A* '(Tennessee Iowa Kentucky North-Carolina Illinois Indiana Wisconsin Ohio))
;((North-Carolina Tennessee Kentucky Ohio Indiana Illinois Wisconsin Iowa) ((2 4) (1 2) (4 8) (5 6)))
;(A* '(Tennessee Iowa Kentucky North-Carolina Missouri))
;((Iowa Missouri Kentucky Tennessee North-Carolina) ((1 4) (1 5) (1 2)))
(define (A* inputState)
   (if (null? inputState)
	(list inputState '())
	(if (is-Components-Connected? inputState) 				
		;checks if the #ConnectedComponent in the inputState is 1, 
		;          #t ==> find the solution 
		;	   #f ==> no way to find the solution, terminate  
		(let 
			((initFrontierList (list (list inputState '() '(0))))) 		;==>   ( inputStateList swapList A*-Value(init:0) ) ==> '(((Louisiana Texas Oklahoma Kansas) (()) 0))
			(A*Wrapper initFrontierList 0)  				;pass to A*wrapper to find the solution through heuristics
		)
		#f
	)
   )
)

;Format of frontierList (((Arizona Alabama Alaska) ((1 2)(1 3)) (2))
(define (A*Wrapper frontierList iteration)
	;(display  iteration)
	(cond
		((null? frontierList) #f)		
		((is-goal-state? (car (car frontierList))) 				;checks if the head of the frontier list is a  goal state
				;(begin (display "Iteration# ")
				;       (display  iteration)(newline)
				       (getListUntilPosition (car frontierList) 1 2)
				;)
		)    
		(#t     (let
				((frontierList (deleteFirstAndAppendChildToTheFrontAndReorderTheList frontierList (get-children (car frontierList))) )) 
				;above line updates the frontier list with the children and keeps the list having lowest A* value at the front.
				(A*Wrapper frontierList (+ iteration 1))
		      	)
		)
	)

)	

;returns the h value of the current  head in the frontier indicating the #swaps to be performed for goal state
;(getValueWrapper '(Tennessee Iowa Missouri North-Carolina Kentucky))
;3	==> 3 states needs to swapped for reaching goal state
;(getValueWrapper '(North-Carolina Tennessee Kentucky Missouri Iowa))
;0	==> All states are in perfect order  for reaching goal state
;returns the number of states that needs to be connected to achieve Goal state (All states are adjacent)
;'(Oregon California Nevada Washington Arizona) ==> 2 {Nevada ->X Washington} {Washington ->X Arizona) ==> 2 states are not adjacent to each other
;'(Arizona California Nevada Oregon Washington)	==> 0 ==> All states are adjacent
(define (getHValueWrapper frontierHead)
	(cond
		((null? frontierHead) 0)
		(#t 
			(let
				((state1 (nth-item 1 frontierHead)) (state2 (nth-item 2 frontierHead)))
				(if (is-adjacent? state1 state2) (getHValueWrapper (cdr frontierHead)) (if (null? state2) 0 (+ 1 (getHValueWrapper (cdr frontierHead))))) 
			)
		)
	)
)

;returns the A* value of current head in frontier, g + h 
(define (getValue headOfTheFrontier)
	;(display  headOfTheFrontier)
	(let
		((g (getLengthOfTheInputList (car (cdr headOfTheFrontier)))) (h (getHValueWrapper (car headOfTheFrontier))))
		;g -> number of steps taken for the current ordering
		;h -> number required to arrive goal state
		(+ g h)
	)
)

;returns the A* values of the frontier list if they dont have one.
;(getA*ValueOfTheFrontierList '(((Oregon Arizona California) ((1 2) (2 3))) ((Louisiana Texas Oklahoma Kansas) (()))))
;(((Oregon Arizona California) ((1 2) (2 3)) 3) ((Louisiana Texas Oklahoma Kansas) (()) 0))
(define (getA*ValueOfTheFrontierList frontierList)
	(cond
		((null? frontierList) '())
		(#t  (let
			    ((head (car frontierList)))
			    (let
				   ((a*Value (nth-item 3 head)))
				   (append (if (null? a*Value) 						;if A* value == null, find A* value else just add the frontier to the result
							(list (append head (list  (getValue head)))) 
							(list head)			     
					   ) 
					   (getA*ValueOfTheFrontierList (cdr frontierList))  ;recurse for rest of frontier
				   ) 
			    )
		     )
		)
	)	
)

;Do dfs for depth upto n-1; n = number of states in the input list
(define (dfsHelper inputState currentDepth maxDepth wildcardSet)
	(cond
		((> currentDepth maxDepth) #f)							;#f ==> dfs has been iterated for max depth and no solution has been found.
		(#t	(let 
				((currentDepthOutput (dfs inputState currentDepth wildcardSet)))				
		       		(if (null? currentDepthOutput) 					
					(dfsHelper inputState (+ currentDepth 1) maxDepth wildcardSet);currentDepthOutput == '() recurse dfs for currentDepth + 1
					currentDepthOutput					      ; 	    	  != '() return the output state
				)
			)
		)
	)
)




;(dfs '(((Oregon Arizona) ())) 2 #f)
(define (dfs frontierList maxDepth wildcardSet)
	;(display currentDepth)(newline)
	(cond
		((null? frontierList) '())
		((> (getLengthOfTheInputList (car (cdr (car frontierList)))) maxDepth) 		;finds if (length of the swap list in the head of the frontier indicating currentDepth > maxDepth)
			(let
				((frontierList (cdr frontierList) ))				;discard the current head as it has been passed the maxDepth 
				(dfs frontierList maxDepth wildcardSet) 			;recurse for rest of the frontier
			)
		)
		((is-goal-state? (car (car frontierList))) (car frontierList))			;if attained goal state, return the state
		(#t     (if wildcardSet								;if wildcard set or not in the input
				(let 
					(( linkStates (checkConnectednessOfStates (car frontierList))));returns if a linkState is found or not for the current head of the frontier
					(if (equal? linkStates '())  
						(let						;if linkState not found, delete the parent, append the children to head and recurse 
							((frontierList (deleteFirstAndAppendChildToTheFrontOfTheList frontierList (get-children (car frontierList))) ))
							(dfs frontierList maxDepth wildcardSet)
						)
						linkStates					;if linkState found, return the output with the wildcarded link state
					)	
		      		)
		      		(let
					((frontierList (deleteFirstAndAppendChildToTheFrontOfTheList frontierList (get-children (car frontierList))) ))
					(dfs frontierList maxDepth wildcardSet) 		;delete the parent, append the children to head and recurse
		      		)
			)
		)
	)
)


;Check if all the states in the input can be reached by atleast any 1 state
;if all can be reached, then it is connected and we are sure of getting a solution through swapping
;if not, all swaps will not lead to solution, hence terminate by returining #f 

;(is-Components-Connected? '(North-Carolina Tennessee))
; #t
;Tennesse can be reached by North-Carolina, 1 connected component and hence #t

; (is-Components-Connected? '(North-Carolina Tennessee Iowa))
; #f
;Iowa cannot be reached either by North-Carolina nor by Tennesse ==> Two components, hence #f
(define (is-Components-Connected? inputState)
	(let 
		((head (car inputState) ))
		(if (> (getLengthOfTheInputList (markVisitedForTheCurrentStateListAndReturnTheUnVisitedStates (list head) (cdr inputState))) 0) #f #t)
		;Above line,finds the number of connected components, 
		;Result == 1 ==> Any states can be reached atleast by any other state in the list       -> #t
		;        > 1 ==> more than 1 disconnected component/s, there exist islands,  if so 	-> #f
	) 
)




;For wildcard, Connectedness involves finding if there is atleast one state that can link the disconnected two statesList 
;==> (State1, state2,....,StateM ) -> Wildcard_State -> (State3, State4,...,StateN)
;Here we allow, 2 connected components to exist inorder to have wildcard location, ==>{ Component1 -> Wildcard_Location -> Component2 }
;Result  ==  2, #t
;	 !=  2, #f

;(is-Components-Connected-Wildcard? '(Oregon Arizona))
;#t  
;(Oregon -> (Nevada || California) -> Arizona) , we have two components, hence wildcard (Nevada || California) will link Oregon and Arizona, hence #t

;(is-Components-Connected-Wildcard? '(Oregon Arizona Utah))
;#f
;Oregon, Arizona, Utah  ==> 3 connected components, We cannot have Wildcard to link 3 connected components, hence #f
(define (is-Components-Connected-Wildcard? inputState)
	(let 
		((head (car inputState) ))
		(let 
			((unVisitedList (markVisitedForTheCurrentStateListAndReturnTheUnVisitedStates (list head) (cdr inputState)))) ;returns unVisitedStatesList from the 1st component
			(if (= (getLengthOfTheInputList unVisitedList)  0) 
				#t 												;there exist only one component, all of them are connected. 				
				(let 												;checks if there exist > 2 components 
					((unVisitedIn2ndComponent (markVisitedForTheCurrentStateListAndReturnTheUnVisitedStates (list (car unVisitedList)) (cdr unVisitedList)) ))
					(if (= (getLengthOfTheInputList unVisitedIn2ndComponent)  0)				;checks if there is no more unvistied states == two components 
						(checkIfAWildcardLocationExistsBetween? inputState unVisitedList)		;|- == 2, checks if a wildcard location exist between those two components
						#f										;|           |--> Wildcard location     exist ==> #t	
																;|	       	                    Not exist ==> #f
					)											;|-  > 2 components, we cannot link           ==> #f
				)			
			)
		)
		
	) 
)

;(checkIfAWildcardLocationExistsBetween? '(Tennessee Iowa Missouri North-Carolina Kentucky Utah Idaho Colorado) '(Utah Idaho Colorado))
;#t
;Link: {Colorado} -> Kansas (Wildcard) -> {Missouri , Colorado -> Nebraska (Wildcard) -> Iowa}  

;(checkIfAWildcardLocationExistsBetween? '(Tennessee North-Carolina Utah Idaho Kentucky Colorado) '(Utah Idaho Colorado))
;#f
;No Link between {Tennessee North-Carolina Kentucky} {Utah Idaho Colorado}
(define (checkIfAWildcardLocationExistsBetween? inputState unVisitedStates)
	(let 
		((visitedStates (removeStatesInTheTargetList unVisitedStates inputState)))			;gets the visites states by (inputState - unVisitedList)
		(findIfAWildcardLocationExistsBetween? visitedStates unVisitedStates)				;checks If there is a  wildcard location between two components
	)
)

;finds  if a wildcard location exist between one Component and other
;(findIfAWildcardLocationExistsBetween? '(Oregon) '(Arizona Utah))
;#t ==> {Oregon} -> (Nevada || Idaho) -> {Utah Arizona}

;(findIfAWildcardLocationExistsBetween? '(Washington) '(Arizona))
;#f ==> {Washington} -> XXXXXXXX -> {Arizona}
(define (findIfAWildcardLocationExistsBetween? visitedStates unVisitedStates)					
	(cond
		((null? visitedStates) #f)
		((null? (checkIfThereExistsAStateAdjacentBetween (car visitedStates) unVisitedStates)) 
			(findIfAWildcardLocationExistsBetween? (cdr visitedStates) unVisitedStates)
		)
		(#t #t)
	)
)

;checks if there exist a state common between one state and other states List
;(checkIfThereExistsAStateAdjacentBetween 'Arizona '(Oregon Idaho))			There exist (Nevada || Utah || California)) between those two
;#t
;(checkIfThereExistsAStateAdjacentBetween 'Texas '(Oregon Idaho))			There doesnt exist between those two, hence '()
;()
(define (checkIfThereExistsAStateAdjacentBetween currentState alternateStatesList)				
	(cond 
		((null? alternateStatesList) '())
		((null? (findAdjacentStateBetweenStates currentState (car alternateStatesList))) 
			(checkIfThereExistsAStateAdjacentBetween currentState (cdr alternateStatesList))
		)
		(#t #t)
	)
)

;returns the disconnected states among a set of states
;(markVisitedForTheCurrentStateListAndReturnTheUnVisitedStates '(Oregon Nevada Arizona)       '()) -> '()      ==> All states in the list are visited 
;(markVisitedForTheCurrentStateListAndReturnTheUnVisitedStates '(Oregon Nevada Arizona Texas) '()) -> '(Texas) ==> Texas cannot be reached by any other states in the list
(define (markVisitedForTheCurrentStateListAndReturnTheUnVisitedStates currentStateList unVisitedStates)		
	(cond 
		((null? currentStateList)  unVisitedStates)
		(#t (let
			((result (getStatesReachableFromCurrentStateListResultAggregator currentStateList unVisitedStates)))   ;results -> '((visitedList) (unVisitedList))
			(markVisitedForTheCurrentStateListAndReturnTheUnVisitedStates (nth-item 1 result) (nth-item 2 result))
		    )
		)
	)
)

;returns a list containing a set of visited states and unvisited state from the input list
;(getStatesReachableFromCurrentStateListResultAggregator '(Oregon Utah) '(Nevada Arizona Texas))
;((Nevada Arizona) (Texas))  ==> Nevada and Arizona were reached by Oregon and Utah leaving Texas unVisited
(define (getStatesReachableFromCurrentStateListResultAggregator currentStateList unVisitedStates)
	(let
		((result (getStatesReachableFromCurrentStateList currentStateList unVisitedStates) ))
		(getVisitedAndUnvisitedStatesFromResult result '() '()) 
	)
)


;(getVisitedAndUnvisitedStatesFromResult '(((Nevada) (Arizona Texas)) ((Arizona) (Texas))) '() '())
;((Nevada Arizona) (Texas))  ==>  Nevada and Arizona were reached by Oregon and Utah leaving Texas unVisited
;This function was used by the aggregator  to combine the outputs from the (getStatesReachableFromCurrentStateList )
(define (getVisitedAndUnvisitedStatesFromResult result visitedList unVisitedList)
	(cond
		((null? result) '())
		((null? (cdr result)) (append (list (append visitedList (nth-item 1 (car  result)))) (list (nth-item 2 (car  result)))))
		(#t (getVisitedAndUnvisitedStatesFromResult (cdr result) (append visitedList (nth-item 1 (car  result))) '()))
	)
)

;returns the set of states that can be visited from an input state list by other states list
;(getStatesReachableFromCurrentStateList '(Oregon Utah) '(Nevada Arizona Texas))
;(((Nevada) (Arizona Texas)) ((Arizona) (Texas)))   ==> Nevada and Arizona were reached by Oregon and Utah leaving Texas unVisited
;The above output is fed to the result aggregator to get visited and unvisited state in a list format like ((Nevada Arizona) (Texas))
(define (getStatesReachableFromCurrentStateList currentStateList unVisitedStates)
	(cond
		((or (null? currentStateList) (null? unVisitedStates))  '())
		(#t (let
			((currentResult (getStatesReachableFromCurrentState (car currentStateList) unVisitedStates) ))
			(append (list currentResult) (getStatesReachableFromCurrentStateList (cdr currentStateList) (nth-item 2 currentResult)))
		     )
		)
	)
)


;(getStatesReachableFromCurrentState 'Oregon '(Nevada Arizona))
;((Nevada) (Arizona))  ==>  Nevada was reached  from Oregon, Leaving Arizona unVisited yet
(define (getStatesReachableFromCurrentState currentState unVisitedStates)
	(let
		((adjacencyListOfTheCurrentState (getAdjacentStatesTo 1 currentState)))
		(let 
			((visitedStates (getAListOfStatesVisited adjacencyListOfTheCurrentState unVisitedStates)))
			(append (list visitedStates) (list (removeStatesInTheTargetList visitedStates unVisitedStates)))
		)
	)
)

;(removeStatesInTheTargetList '(Utah Idaho Texas) '(Tennessee Iowa Missouri North-Carolina Kentucky Utah Idaho Texas))
;'(Tennessee Iowa Missouri North-Carolina Kentucky)
(define (removeStatesInTheTargetList toBeRemovedList targetList)
	(cond
		((null? toBeRemovedList) targetList)
		((equal? (car toBeRemovedList) (car targetList)) (removeStatesInTheTargetList (cdr toBeRemovedList) (cdr targetList)))
		(#t (append (list (car targetList)) (removeStatesInTheTargetList toBeRemovedList (cdr targetList)) ))
	)	
)

;(getAListOfStatesVisited '(Arizona Nevada Oregon) '(Utah Arizona Texas))
;(Arizona)    ==> Arizona can be visited from this adjacency list of California
(define (getAListOfStatesVisited adjacencyList inputStateList)
	(cond
		((null? inputStateList) '())
		(#t (append  (if (findIfStatePresentInTheAdjacencyList? (car inputStateList) 1 adjacencyList) 
					(list (car inputStateList)) 
					'()
			     ) 
			     (getAListOfStatesVisited adjacencyList (cdr inputStateList))
		    )
		)
        )
)

;(deleteFirstAndAppendChildToTheFrontOfTheList '((Oregon Arizona)((1 2))) '((((Arizona Alabama Alaska) ((1 2))) ((Alaska Arizona Alabama) ((1 3))) ((Alabama Alaska Arizona) ((2 3))))))
;((((Arizona Alabama Alaska) ((1 2))) ((Alaska Arizona Alabama) ((1 3))) ((Alabama Alaska Arizona) ((2 3)))) ((1 2)))
(define (deleteFirstAndAppendChildToTheFrontOfTheList inputList appendList)
 		(append  appendList (cdr inputList))
)


;deletes the head of the frontier  list, appends  the children to the head, reorders by keeping the lowest A* value and returns the list
(define (deleteFirstAndAppendChildToTheFrontAndReorderTheList inputList appendList)
	(let
		((appendedFrontierList (append  appendList (cdr inputList))))				 ;deletes the head of the frontier  list, appends  the children to the head
		(let 
			((frontierListWithA*Values (getA*ValueOfTheFrontierList appendedFrontierList) )) ;gets the frontier list with A* values
	       		(swapTheFrontierListBasedOnA*Value frontierListWithA*Values)			 ;sets the head of the frontier with the one having lowest A* value
		)
	)
)

;returns the position having the smallest A* value in the frontier list
(define (find-SmallestPositionHelper number currList smallestPosition runningPosition)			
	(cond
		((null? currList)  smallestPosition)							;if list == null, return the current biggest
		(#t 	(let 
				((head  (nth-item 3 (car currList)))) 					;retrieve head from the input list
				(if (<= number head) 
					(find-SmallestPositionHelper number (cdr currList) smallestPosition (+ runningPosition 1)) 
					(find-SmallestPositionHelper head (cdr currList) runningPosition (+ runningPosition 1)) 
				)
			)
		)
	)
)

;returns the position having the smallest A* value
(define (returnPositionOfTheSmallestA*Value inputFrontierList)						 
	(cond
		((null? inputFrontierList) '())    							;if list is null, return null
		(#t (find-SmallestPositionHelper (nth-item 3 (car inputFrontierList)) (cdr inputFrontierList) 1 2));else pass onto a wrapper to get the biggest assigning first number in the list as the biggest 
	)
)

;replaces frontier list element having the lowest A* value with frontier's head
(define (swapTheFrontierListBasedOnA*Value inputFrontierList)									
	(let
		((targetPosition (returnPositionOfTheSmallestA*Value inputFrontierList) ))
	        (swap-elements 1 targetPosition inputFrontierList)
	)
)

;return the position of the Wildcard in the  input state
;(locateWildcard 1 '(Tennessee Iowa Kentucky Wildcard North-Carolina Illinois Indiana Wisconsin Ohio))
;4
(define (locateWildcard position frontierList)
	(cond
		((null? frontierList) '())
		((equal? (car frontierList) 'Wildcard) position)
		(#t (locateWildcard (+ position 1) (cdr frontierList)))
	)
)

;checks if the states are connected through bridging wildcard location if so returns the list with bridge if not '()
;(checkConnectednessOfStates '((Iowa Wildcard Tennessee Kentucky) ((1 2))))
;(((Iowa (Wildcard Missouri) Tennessee Kentucky) ((1 2))))
;(checkConnectednessOfStates '((Wildcard Iowa Tennessee Kentucky) ()))
;()
(define (checkConnectednessOfStates headfrontierList)
	(let 
		((result (checkWildcardForPosition (locateWildcard 1  (car headfrontierList)) (car headfrontierList))))
		(cond 
			((null? result)  '())									;could not be bridged through wildcard location
			(#t 
				(let
					((linkLocations (findAdjacentStateBetweenStates (nth-item 2 result) (nth-item 3 result)))) 		;finds the wildcard locations and bridges two components
					(getPathLinkedWithWildcardLocations (car headfrontierList) (nth-item 1 result) linkLocations (cdr headfrontierList))
					;above line links the wildcard location with the current head of the frontier list and returns the feasible solutions 
				)
			)
		)
	)
)

;returns the frontier list linked with the wildcard location
;(getPathLinkedWithWildcardLocations '(Iowa Wildcard Tennessee Kentucky) 2 '(Missouri) '((1 2)))
;(((Iowa (Wildcard Missouri) Tennessee Kentucky) (1 2)))
(define (getPathLinkedWithWildcardLocations inputList linkPosition linkStateList swapList)
	(let
		((partition1 (getListUntilPosition inputList 1 (- linkPosition 1))) (partition2 (getListFromPosition inputList 1 (+ linkPosition 1))))
		(cond
			((null? linkStateList) '())
			;((checkDuplicatesInBothThePartitions (car linkStateList) partition1 partition2) '())
			(#t (append (list (linkStateBetweenTwoPartitions partition1 (car linkStateList) partition2 swapList))  ;((partition1 - Wildcard_location - partition2)(swapList))
				     (getPathLinkedWithWildcardLocations inputList linkPosition (cdr linkStateList) swapList)  ;recurse for rest of the wildcard locations '((wildcarded-1)(wildcarded-2)...)
			    )
			)
		)
	)
)


;links partition1 partition2 with the wildcarded state  ==> ((partition1 - Wildcard_location - partition2)(swapList))
(define (linkStateBetweenTwoPartitions partition1 linkState partition2 swapList)
	 (append (list (append partition1 (append (list (list 'Wildcard linkState)) partition2))) swapList)
)

;finds the adjacentStates between two states or returns '()
;(findAdjacentStateBetweenStates 'Nevada 'Arizona)
;(California Utah)
;California and Utah are the states adjacent to Nevada and Arizona

;(findAdjacentStateBetweenStates 'Nevada 'Texas)
;()
;No adjacent states between Nevada and Texas
(define (findAdjacentStateBetweenStates state1 state2)
	(cond
		((or (null? state1) (null? state2)) '())
		(#t  (let 
				((adjacentStatesOfState1 (getAdjacentStatesTo 1 state1)) (adjacentStatesOfState2 (getAdjacentStatesTo 1 state2))) ;gets the adjacency list of two states
				(findIntersectionAmongStates adjacentStatesOfState1 adjacentStatesOfState2)			      ;finds the common states between two states's adjacency list
			)
		)
	)	
)

;checks if the linkState is already present in either of the two components if so we discard that solution
;(checkDuplicatesInBothThePartitions 'California '(Oregon) '(Arizona California Arizona))
;#t
;linking state California is present in 2nd partition hence we will not use that wildcard location 
(define (checkDuplicatesInBothThePartitions linkState partition1 partition2)
	(or (checkDuplicateFor linkState partition1) (checkDuplicateFor linkState partition2))
)

;checks duplicate for the linking state in the partition list
(define (checkDuplicateFor linkState partitionList)
	(findIfStatePresentInTheAdjacencyList? linkState 1 partitionList)
)	

;returns the intersection states betweeen two state's adjacency list or '()
;(findIntersectionAmongStates '(Washington Idaho Nevada California) '(California Nevada Utah New-Mexico))
;(Nevada California)  ==> Adjacent states to Oregon and Arizona 
(define (findIntersectionAmongStates adjacentToState1 adjacentToState2)
	(cond
		((null? adjacentToState1) '())
		((null? adjacentToState2) '())
		(#t  (if (findIfStatePresentInTheAdjacencyList? (car adjacentToState1) 1 adjacentToState2) 
			(append (list (car adjacentToState1)) (findIntersectionAmongStates (cdr adjacentToState1) adjacentToState2)) ;recurse for rest of the states in the adjacency list
				(findIntersectionAmongStates (cdr adjacentToState1) adjacentToState2)
		      )
		)
	) 
)


;checks if a wildcard location is possible for the current head of the frontier list
; if  so,  returns the linking boundary states and the position
; else returns null

;(checkWildcardForPosition 2 '(Iowa Wildcard Tennessee Kentucky))
;(2 Iowa Tennessee)
;At position 2, a wildcard location is possible between Iowa and Tennessee

;(checkWildcardForPosition 3 '(Tennessee Iowa  Wildcard Kentucky))
;()
;No such wildcard location if possible for  current ordering
(define (checkWildcardForPosition position inputList)
	(cond
		((> position (getLengthOfTheInputList inputList)) '())
		((and (is-goal-state? (getListUntilPosition inputList 1 (- position 1))) (is-goal-state? (getListFromPosition inputList 1 (+ position 1)))) 
			(list position (nth-item (- position 1) inputList) (nth-item (+ position 1) inputList))
		)
		;above line checks, if two components are connected,then insert wildcard,  else return null
		(#t '())
	)
)



;(is-adjacent? 'Florida 'Georgia)
;#t
(define (is-adjacent? state1 state2)
	(let
		((adjacentListOfState1 (getAdjacentStatesTo 1 state1)))					;get the adjacent states list of State1
		(findIfStatePresentInTheAdjacencyList? state2 1 adjacentListOfState1)			;checks if state 2 is present in the adjacency list of state1
	) 
)

;gets the adjacent states of the given input state
;(getAdjacentStatesTo 1 'California)
;(Arizona Nevada Oregon)
(define (getAdjacentStatesTo row inputState)									
	(let 
		((eachStateAdjacencyMap (nth-item row adjacency-map)))						;finds if the state is present on any of the rows
		(cond
			((null? eachStateAdjacencyMap) '())							;returns '()          if not found
			((equal? inputState (nth-item 1 eachStateAdjacencyMap))  (cdr eachStateAdjacencyMap))	;returns the row, if found 
			(#t (getAdjacentStatesTo (+ row 1) inputState))						;recurses for the rest of the states
		)
	)
)

;checks if the state is present in the adjacency list of other state if so #t else #f
;(findIfStatePresentInTheAdjacencyList? 'California 1 '(California Nevada Utah New-Mexico))
;#t 
(define (findIfStatePresentInTheAdjacencyList? state column inputList)				
	(cond
		((null? inputList)               #f)					        ;returns false  if not found
		((equal? state (car inputList))  #t)						;returns true   if found 
		(#t (findIfStatePresentInTheAdjacencyList? state (+ column 1) (cdr inputList)))	;recurses for the rest of the column elements
	)
)




;(is-goal-state? '(North-Carolina Tennessee Kentucky Missouri Iowa))
;(Iowa Tennessee Kentucky North-Carolina Missouri)
;(Iowa North-Carolina Tennessee Kentucky Missouri)
;(Missouri Tennessee Kentucky North-Carolina Iowa)
;(is-goal-state? '(North-Carolina Tennessee Kentucky Missouri Iowa))
(define (is-goal-state? inputStateList)
	(cond
		((null? inputStateList) #t)
		(#t 
			(let
				((state1 (nth-item 1 inputStateList)) (state2 (nth-item 2 inputStateList)))		;gets the state1 and state2
				(if (is-adjacent? state1 state2) 
					(is-goal-state? (cdr inputStateList)) 						;if both states are adjacent, recurse for rest of the states
					(if (null? state2) 							        ;		not adjacent, check if it is the last state, then #t, else #f
						#t 
						#f
					)
				)
			)
		)
	)
)

;gets the children for the current inputList
;(get-children '((Alabama Arizona Alaska) ()))
;(((Arizona Alabama Alaska) ((1 2))) ((Alaska Arizona Alabama) ((1 3))) ((Alabama Alaska Arizona) ((2 3))))
(define (get-children inputList)
	(let 
		((stateList (car inputList)) 
		 (swapList (if (null? (car (cdr inputList))) 
				'() 
				(if (null? (car (car (cdr inputList)))) 
					'() 
					(car (cdr inputList))
				)
			   )
		))
	        (getChildFor 1 (getLengthOfTheInputList stateList) stateList swapList)		;helper to get children that takes the current state and swapList
	)
)

;(getChildFor 1 3 '(Alabama Arizona Alaska) '())
;(((Arizona Alabama Alaska) ((1 2))) ((Alaska Arizona Alabama) ((1 3))) ((Alabama Alaska Arizona) ((2 3))))
(define (getChildFor index1 limit inputList swapList)
	(callSwapWithIncIndex1 index1 (+ index1  1) limit inputList swapList)
)

;helper1 to calculate the child
(define (callSwapWithIncIndex1 index1 index2 limit inputList swapList)
	(cond 
		((= index1 limit) '())
		(#t
			 (append (callSwapWithIncIndex2 index1 index2 limit inputList swapList) 
				 (callSwapWithIncIndex1 (+ index1 1) (+ index1 2) limit inputList swapList)
			 )
		)
	)
)

;helper2 to calculate the child
(define (callSwapWithIncIndex2 index1 index2 limit inputList swapList)
	(cond 
		((> index2 limit) '())
		(#t	
			(append 
				(list 
					(list 
						(swap-elements index1 index2 inputList) 
						(append swapList  
								(list (list index1 index2))
						)
				      )
				)  
				(callSwapWithIncIndex2 index1 (+ index2 1) limit inputList swapList)
			)
		)
	)
)



;replaces nth item in the list with the replacement list
;(replace-nth-item 2 '(Oregon Wildcard Arizona) '(Wildcard California))
;(Oregon (Wildcard California) Arizona)
(define (replace-nth-item replacementPosition inputList replacementList)
	;(newline)
        (cond
                ((or (< replacementPosition 1) (null? inputList)) "Invalid index! or Input list is null") ;inputList == null || position < 1 ==> throw error
                (#t  (replaceNthItemHelper 1 replacementPosition inputList  (list replacementList)))       ;call Helper to replace the item at specified position
        )
)

;helper for the above function
(define (replaceNthItemHelper currentPosition replacementPosition inputList replacementList )
	(cond
                ((null? inputList) '())                                                                   ;inputList == null, return null list
                ((= currentPosition replacementPosition) (append replacementList (cdr inputList)))        ;position matched, replace the item at the current position and append it with (cdr list)
                (#t (append (list (car inputList)) 
		            (replaceNthItemHelper (+ 1 currentPosition) replacementPosition (cdr inputList) replacementList)
		    )
		)
                ;recurse with appending the head of the list with the rest of the list containing the replaced item at head of the list
        )
)

;returns the nth-item in the list
;(nth-item 2 '(Oregon Arizona))
;Arizona
(define (nth-item n inputList)
	(cond
                ((or (< n 1) (null? inputList)) '())                    				;inputList == null or position < 1 ==> throw  error
                (#t  (getNthItemHelper 1 n inputList)) 							;result == null ==> Position is beyond the list size 
	)												;result != null ==> Return the element found at the position
)

;helper for the above function
(define (getNthItemHelper currentPosition requiredPosition inputList)
        (cond
                ((null? inputList) '())                                                                 ;inputList == null ==> return '()
                ((= currentPosition requiredPosition) (car inputList))                                  ;required position == currentPosition return the car of the list
                (#t (getNthItemHelper (+ 1 currentPosition) requiredPosition (cdr inputList)) )         ;recurse until required position == currentPosition
        )
)

;(swap-elements 2 3 ‘(a b c d))
;(a c b d)
(define (swap-elements index1 index2 inputList)
	(let  
		((value1  (nth-item index1 inputList)) (value2  (nth-item index2 inputList)))
		(let
				((inputList (replace-nth-item index1 inputList  value2)))
			        (replace-nth-item index2 inputList value1)
		)	
	)
)

;returns the list till the specified position
;(getListUntilPosition '(Oregon Arizona California) 1 2)
;(Oregon Arizona)
(define (getListUntilPosition inputList currentPosition requiredPosition)
	(cond 	
		((= (- currentPosition 1) requiredPosition) '())
		(#t (append (list (car inputList)) (getListUntilPosition (cdr inputList) (+ currentPosition 1) requiredPosition)))
	)
)

;returns the list from the specified position
;(getListFromPosition '(Oregon Arizona California) 1 2)
;(Arizona California)
(define (getListFromPosition inputList currentPosition requiredPosition)
	(cond 	
		((= currentPosition requiredPosition) inputList)
		(#t (getListFromPosition (cdr inputList) (+ currentPosition 1) requiredPosition))
	)
)

;gets the length of the inputlist
;(getLengthOfTheInputList '(a b c))
;3
(define (getLengthOfTheInputList inputList)
	(cond	
		((or (null? inputList) (null? (car inputList))) 0)
		(#t (+ 1 (getLengthOfTheInputList (cdr inputList))))
	)
)

;displays each list row by row
(define (showList inputList)							
	(cond 
		((null? inputList) '())
		(#t	(begin	
				(display (car inputList))(newline)
				(showList (cdr inputList))
		   	)
		)
	)		
)




(define adjacency-map '(
  (Alabama Mississippi Tennessee Georgia Florida)
  (Alaska)
  (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
  (Arizona California Nevada Utah New-Mexico)
  (California Arizona Nevada Oregon)
  (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
  (Connecticut New-York Massachusetts Rhode-Island)
  (Delaware Maryland Pennsylvania New-Jersey)
  (Florida Alabama Georgia)
  (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
  (Hawaii)
  (Idaho Oregon Washington Montana Wyoming Utah Nevada)
  (Indiana Illinois Michigan Ohio Kentucky)
  (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
  (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
  (Kansas Colorado Nebraska Missouri Oklahoma)
  (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
  (Louisiana Texas Arkansas Mississippi)
  (Maine New-Hampshire)
  (Maryland Virginia West-Virginia Pennsylvania Delaware)
  (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
  (Michigan Wisconsin Indiana Ohio)
  (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
  (Mississippi Louisiana Arkansas Tennessee Alabama)
  (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
  (Montana Idaho Wyoming South-Dakota North-Dakota)
  (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
  (Nevada California Arizona Utah Idaho Oregon)
  (New-Hampshire Maine Vermont Massachusetts)
  (New-Jersey Delaware Pennsylvania New-York)
  (New-Mexico Texas Oklahoma Colorado Arizona)
  (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
  (North-Carolina South-Carolina Georgia Tennessee Virginia)
  (North-Dakota Montana South-Dakota Minnesota)
  (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
  (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
  (Oregon Washington Idaho Nevada California)
  (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
  (Rhode-Island Connecticut Massachusetts)
  (South-Carolina Georgia North-Carolina)
  (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
  (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
  (Texas New-Mexico Oklahoma Arkansas Louisiana)
  (Utah Nevada Idaho Wyoming Colorado Arizona)
  (Vermont New-York Massachusetts New-Hampshire)
  (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
  (Washington Oregon Idaho)
  (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
  (Wisconsin Minnesota Iowa Illinois Michigan)
  (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
))


