

:-module(fRAgBlackboard,[
			    frag_debug / 1,
			    agent / 1,
			    ready / 1,
			    go / 1
			]
	).


/** <module>  FRAg Environment Interface

Blackboard for distributed communication

@author Frantisek Zboril
@license GPL
*/



:-dynamic frag_debug / 1.
:-dynamic agent /1.				
:-dynamic ready /1.
:-dynamic go / 1.
:-dynamic max_agent_iterations /1.

