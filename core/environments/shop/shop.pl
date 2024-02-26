

:-module(card_shop,
    [
        card_shop / 2,
	card_shop / 3,
	card_shop / 4
    ]
).


/**
@Author Frantisek Zboril jr.
@version 0.5 (2023) 
*/


:- discontiguous card_shop/2.
:- discontiguous card_shop/3.
:- discontiguous card_shop/4.

time_adjust_multiply(30).

% :- dynamic number_of_products /1.
% :- dynamic mean_product_price /1.
% :- dynamic dispersion_product_price /1.
:- dynamic product /2.
:- dynamic episode /1.
:- dynamic average_sellers /1.
:- dynamic average_buyers /1.
:- dynamic mean_discount_buyer /1.
:- dynamic mean_discount_seller /1.
:- dynamic dispersion_discount_buyer /1.
:- dynamic dispersion_discount_seller /1.
:- dynamic buyer_stay /1.
:- dynamic seller_stay /1.
:- dynamic closing_time /1.
:- dynamic mean_discount_seller /1.
:- dynamic episode_length /1.
:- dynamic buyers /1.
:- dynamic sellers /1.
:- dynamic previous_time /1.
:- dynamic episode_mode /1.
:- dynamic episode_length /1.



:-use_module('../FRAgPLEnvironmentUtils').   % interface to environments
:-use_module('stat_utils').


episode(1).

max_price(160).

product(cd1, 100).
product(cd2, 80).
product(cd3, 68).
product(cd4, 110).
product(cd5, 50).
product(cd6, 90).
product(cd7, 110).
product(cd8, 55).
product(cd9, 150).



% environment(card_shop).

% number_of_products(4).
% mean_product_price(100).
% dispersion_product_price(20).
average_buyers(0.22).
average_sellers(0.22).
mean_discount_buyer(0.6).
dispersion_discount_buyer(0.2).
mean_discount_seller(0.4).
dispersion_discount_seller(0.2).
buyers_stay(100).
sellers_stay(100).
closing_time(750).
buyers(0).
sellers(0).
episode_mode(real_time).  % Mode is either sim_time or real_time
episode_length(0.01). % in secs

previous_time(-1).


%!  init_beliefs(+Agents)
%   Inserts beliefs has / price / sells to Agents
%  @arg Agents: List of agents for which beliefs should be initialized

init_beliefs(Agents):-
    findall_environment(card_shop, Agent, has( _, _), Beliefs1),
    findall_environment(card_shop, Agent, product( _, _), Beliefs2),
    findall_environment(card_shop, Agent, sells( _, _, _), Beliefs3),
    add_beliefs_agents(Agents, Beliefs1),
    add_beliefs_agents(Agents, Beliefs2),
    add_beliefs_agents(Agents, Beliefs3),
    add_beliefs_agents(Agents, [episode(1)]).
                  

%!  card_shop(++Functionality, +Attributes) is det 
%  @arg Functionality is one of 
%*      set_parameters 
%*      add_agent
%*      clone
%*      reset_clone
%*      remove_clone
%  @arg Attributes: List of parameters in the form of tuples
%   1. For functionality 'set_attributes' the attributes can be
%*      (products, [Number, Mean_Price, Dispersion])
%*      (b_lambda, Mean) 
%*      (s_lambda, Mean)
%*      (b_price, [Mean, Dispersion])
%*      (s_price, [Mean, Dispersion]) 	
%*      (closing, Episode)
%*      (b_stay, Episodes)
%*      (c_stay, Episodes)
%*      (episoding, sim_time)
%*      (episoding, (real_time, Time)) 
%   2. For functionality 'add_agant' the parameter is agent's name


card_shop(set_attributes, []).

card_shop(set_attributes, [(Key, Value)| Attributes]):-
    set_parameter(Key, Value),
    card_shop(set_attributes, Attributes).



set_parameter(products, [Number, Mean, Dispersion]):-
%    change_params(number_of_products(Number)),
%    change_params(mean_product_price(Mean_Price)),
%    change_params(dispersion_product_price(Dispersion)),
    retractall(product( _, _)),
    generate_products(Number, Mean, Dispersion).

set_parameter(b_lambda, Mean):-
     change_params(average_buyers(Mean)).

set_parameter(s_lambda, Mean):-
    change_params(average_sellers(Mean)).

set_parameter(b_price, [Mean, Dispersion]):-
    change_params(mean_discount_buyer(Mean)),
    change_params(dispersion_discount_buyer(Dispersion)).

set_parameter(s_price, [Mean, Dispersion]):- 	
    change_params(mean_discount_seller(Mean)),
    change_params(dispersion_discount_seller(Dispersion)).

set_parameter(closing, Episode):-
    change_params(closing_time(Episode)).

set_parameter(b_stay, Episodes):-
    change_params(buyers_stay(Episodes)).

set_parameter(c_stay, Episodes):-
    change_params(sellers_stay(Episodes)).

set_parameter(episoding, sim_time):-
    change_params(episode_mode(sim_time)).  

set_parameter(episoding, (real_time, Time)):-
    change_params(episode_mode(real_time)),
    change_params(episode_mode(Time)).

set_parameter( _, _):-
    format("[ERROR] card shop, wrong parameters").   



change_params(Atom):-
   Atom=..[Predicate, _],
   Retract=..[Predicate, _],
   retract(Retract),
   assert(Atom).

change_params(Atom):-
    assert(Atom).


%!  generate_products(+Number, +Mean_Price, +Dispersion) is det
%   Generates Number of CDs with prices ~N(Mean_Prixe, Dispersion) truncates
%   to tens
%  @arg Number: Number of products to be generated
%  @arg Mean_Price:
%  @arg Dispersion:

generate_products(0, _, _).

generate_products(Number, Mean, Dispersion):-
    format(atom(CD_Name), "cd~w", [Number]),
    term_string(CD, CD_Name),
    max_price(Max_Price),
    get_discount(Mean, Dispersion, Discount),
    CD_Price is truncate(Max_Price * (1 - Discount)),
    assert(product(CD, CD_Price)),
    Number2 is Number -1,
    generate_products(Number2, Mean, Dispersion).

   
 
%!  card_shop(add_agent, +Agent) is det
%   Adds agent Agent to the main instance of card_shop environment
%  @arg Agent: Name of the agent

card_shop(add_agent, Agent):-
    situate_agent_environment(Agent, card_shop),
    init_beliefs([Agent]),
    delete_facts_beliefs_all(card_shop, Agent,
                             [stats_([sold(Sold_By), buyers(Buyers),
                                      sellers(Sellers)])]),

    append(Sold_By, [sold(Agent, 0)], Sold_By2),
    add_facts_beliefs_all(card_shop, Agent,
                          [stats_([sold(Sold_By2), buyers(Buyers),
                                   sellers(Sellers)])]).


%!  card_shop(add_agent, +Instance +Agent) is det
%   Adds agent Agent to the +Instance of card_shop environment
%  @arg Agent: Name of the agent
%  @arg Instance: Insatnce of the card_shop environment

card_shop(add_agent, Agent, Instance):-
    situate_agents_clone([Agent], card_shop, Instance),
    init_beliefs([Agent]).


%!  card_shop(clone, +Instance) is det
%   Creates a clone 
%  @arg Instance: Insatnce of the card_shop environment

card_shop(clone, Instance):-
    clone_environment(card_shop, Instance).


%!  card_shop(reset_clone, +Clone) is det
%   Resets clone to its initial state
%  @arg Clone:

card_shop(reset_clone, Clone):-
    reset_environment_clone(card_shop, Clone),
    get_all_situated(card_shop, Clone, Agents),
    init_beliefs(Agents).


%!  card_shop(remove_clone, +Clone) is det
%   Removes clone instance
%  @arg Clone:

card_shop(remove_clone, Clone):-
    remove_environment_clone(card_shop, Clone).

card_shop(save_state, Instance, State):-
    save_environment_instance_state(card_shop, Instance, State).

card_shop(load_state, Instance, State):-
    load_environment_instance_state(card_shop, Instance, State).

card_shop(remove_state, Instance, State):-
    remove_environment_instance_state(card_shop, Instance, State).


%!  card_shop(perceive, +Agent, -Add_List, - Delete_List) is det
%   Passes changes to the Agent in the form od Add_List and Delete_List
%  @arg Agent: Agent that perceives some instance of card_shop environment
%  @arg Add_List: New percept since last perceiving
%  @arg Delete_List: Disapeared peceps since last perceiving

card_shop(perceive, Agent , Add_List, Delete_List):-
    check_episode(Agent),
    retreive_add_delete(Agent, Add_List, Delete_List),
    query_environment(card_shop, Agent, stats_([sold(Sold_By),
                                                  buyers( _ ), sellers( _ )])),
    delete_facts_beliefs_all(card_shop, Agent,
                             [stats_([sold(Sold_By), buyers( _ ), sellers( _ )])]),
    sellers(S),
    buyers(B),
    add_facts_beliefs_all(card_shop, Agent, [stats_([sold(Sold_By),
                                               buyers(B), sellers(S)])]).


%!  check_episode(+Agent) is det
%   Checks if the episode is over and if so, updates environment.
%   TODO should respect particular instances (now it is for all instances)
%  @arg Agent: agent perceiving environment, could be used for episode checking

%   In the case of sim_time, the check allways passes

check_episode(Agent):-
    episode_mode(sim_time),
    episode(Episode),
    delete_facts_beliefs_all(card_shop, Agent,
                             [episode( Episode )]),
    Episode2 is Episode + 1,
    add_facts_beliefs_all(card_shop, Agent, [episode(Episode2)]),
    update_environment(Agent, 1).    

%   In the case of real_time, 0 to N episodes happened due to time differences
%   between current and last percieved time 

check_episode( _ ):-
    episode_mode(real_time),
    previous_time(-1),
    get_time(Time),
    retract(previous_time( _ )),
    assert(previous_time(Time)).

check_episode(Agent):-
    episode_mode(real_time),
    new_episode_time(Agent, Number),
    episode(Episode),
    delete_facts_beliefs_all(card_shop, Agent,
                             [episode( Episode )]),
    Episode2 is Episode + Number,
    add_facts_beliefs_all(card_shop, Agent, [episode(Episode2)]),
    update_environment(Agent, Number).

check_episode( _ ).


new_episode_time( _ , Number):-
    previous_time(Previous_Time),
    get_time(Time),
    episode_length(Episode_Length),
    !,
    Delta is (Time - Previous_Time),
    Episode_Length < Delta,
    Number is truncate(Delta / Episode_Length),
    retract(previous_time( _ )),
    assert(previous_time(Time)).


%!  update_environment(+Agent, +Number) is det
%   Updates environment Number-times.
%  @arg Agent: For which agent to update the environment
%  @arg Number: Number of updates

update_environment( _, 0).

update_environment(Agent, Number):-
    episode(Episode),
    retractall(episode(Episode)),
    Episode2 is Episode + 1,
    assert(episode(Episode2)),
    patience_out(Agent, Episode2),
    add_customers(Episode2, Agent),
    Number2 is Number-1,
    update_environment(Agent, Number2).



add_customers(Episode , Agent):-
    closing_time(Closing_Time),
    Episode < Closing_Time,
    add_sellers(Agent),
    add_buyers(Agent).

add_customers(Episode, Agent):-
    closing_time(Episode),
    get_time(Time2),
    retract(previous_time( _ )),
    assert(previous_time(Time2)),

    closing_time(Episode),
    add_facts_beliefs_all(card_shop, Agent, [closed]).

add_customers( _, _).



patience_out(Agent, Episode):-
%    findall_environment(card_shop, Agent, deadline(Person, Name, Episode),
%                        Unpatients),
    findall_environment(card_shop, Agent, deadline( _, _, Episode),
                        Unpatients),
    unpatients_left(Agent, Unpatients).



unpatients_left( _, []).

unpatients_left(Agent, [Unpatient | Unpatients]):-
    remove_unpatient(Agent, Unpatient),
    unpatients_left(Agent, Unpatients).



remove_unpatient(Agent, deadline(seller, Seller, _)):-
   delete_facts_beliefs_all(card_shop, Agent,
                              [seller(Seller, _, _)]).
%   add_facts_beliefs_all(card_shop, Agent,
%                             [left(Seller)]).



remove_unpatient(Agent, deadline(buyer, Buyer, _)):-
   delete_facts_beliefs_all(card_shop, Agent,
                             [buyer(Buyer, _, _)]).
%   add_facts_beliefs_all(card_shop, Agent,
%                             [left(Buyer)]).


add_sellers(Agent):-
    average_sellers(Lambda),
    mean_discount_seller(Mean_Seller),
    dispersion_discount_seller(Dispersion_Seller),
    sellers(Seller_Index),
    new_events_number(Lambda, New_Sellers),
    sellers_stay(Stay_Length),
    add_persons(Agent, Lambda, seller, Seller_Index, New_Sellers, Mean_Seller,
                Dispersion_Seller, Stay_Length).

add_buyers(Agent):-
    average_buyers(Lambda),
    mean_discount_buyer(Mean_Buyer),
    dispersion_discount_buyer(Dispersion_Buyer),
    buyers(Buyer_Index),
    new_events_number(Lambda, New_Buyers),
    sellers_stay(Stay_Length),
    add_persons(Agent, Lambda, buyer, Buyer_Index, New_Buyers, Mean_Buyer,
                Dispersion_Buyer, Stay_Length).



add_persons(_, _, _, _, 0, _, _, _).

add_persons(Agent, Lambda, Predicate, Index, N, Mean, Dispersion, Stay_Length):-
    generate_cd_price(CD, Price, Mean, Dispersion),
    Index2 is Index+1,
    N2 is N-1,
    format(atom(Person), "~w~w", [Predicate, Index2]),
    increase_persons(Predicate),
    Fact =..[Predicate, Person, CD, Price],
    add_facts_beliefs_all(card_shop, Agent, [Fact]),
% set deadline ... as a fact only
    episode(E),
    E2 is E + Stay_Length,
    add_facts_agent(card_shop, Agent, [deadline(Predicate, Person, E2)]),
    add_persons(Agent, Lambda, Predicate, Index2, N2, Mean, Dispersion,
                Stay_Length).




increase_persons(buyer):-
    retract(buyers(B)),
    B2 is B+1,
    assert(buyers(B2)).

increase_persons(seller):-
    retract(sellers(S)),
    S2 is S+1,
    assert(sellers(S2)).

generate_cd_price(CD, Price_Out, Mean, Dispersion):-
    findall(cd(CD, Price), product(CD, Price), CDs),
    random_member(cd(CD, Price),CDs),
    get_discount(Mean, Dispersion, Discount),
    Price_Out is truncate(Price * (1 - Discount)).




%    Agent acts


card_shop(act, Brooker, sell(Seller, Buyer, What), true):-
%    query_environment(card_shop, Brooker, seller(Seller, What, Price)),
%    query_environment(card_shop, Brooker, buyer(Buyer, What, Price2)),
    query_environment(card_shop, Brooker, seller(Seller, What, _)),
    query_environment(card_shop, Brooker, buyer(Buyer, What, _)),

    add_facts_beliefs_all(card_shop, Brooker, [has(Buyer, What),
                                               sold(Seller, What)]),
    query_environment(card_shop, Brooker, stats_([sold(Sold_By), buyers(B),
                                                 sellers(S)])),
    delete_facts_beliefs_all(card_shop, Brooker,
                             [stats_([sold(Sold_By), buyers( _ ), sellers( _ )])]),
    delete_facts_beliefs_all(card_shop, Brooker,
                             [buyer(Buyer, What, _)]),
    delete_facts_beliefs_all(card_shop, Brooker,
                             [seller(Seller, What, _)]),

    delete_facts_agent(card_shop, Brooker, [deadline(seller, Seller, _),
                                            deadline(buyer, Buyer, _)]),

    add_trade(Sold_By, Brooker, Sold_By2),
    add_facts_beliefs_all(card_shop, Brooker, [stats_([sold(Sold_By2), buyers(B),
                                               sellers(S)])]).



card_shop(act, _, sell( _, _), false).


card_shop(act, _, sell( Seller, Buyer, What), false):-
   format("Prodej ~w komu ~w co ~w selhal~n",[Seller, Buyer, What]).


add_trade(Sold_By, Seller, Sold_By3):-
    member(sold(Seller, N), Sold_By),
    delete(Sold_By, sold(Seller, N), Sold_By2),
    N2 is N+1,
    append(Sold_By2, [sold(Seller, N2)], Sold_By3).

add_trade(Sold_By, Seller, Sold_By2):-
    append(Sold_By, [sold(Seller, 1)], Sold_By2).



%	Agent AGENTNAME acts silently

  % for mcts

card_shop(act, Seller, silently_(sell(Seller, What)), Result):-
    card_shop(act, Seller, sell(Seller, What), Result).

card_shop(act, Brooker, silently_(sell(Seller, Buyer, What)), Result):-
    card_shop(act, Brooker, sell(Seller, Buyer, What), Result).

card_shop(act, _, _, fail).



:-
    env_utils:register_environment(card_shop),
    findall(product(What, Price), product(What, Price), Facts),
    episode(Episode),
    env_utils:add_facts(card_shop, [stats_([sold([]), buyers(0), sellers(0)])]),
    env_utils:add_facts(card_shop, Facts),
    env_utils:add_facts(card_shop, [episode(Episode)]).

md:-
    use_module(library(pldoc/doc_library)),
    doc_save('shop.pl',[format(html), recursive(true), 
                                          doc_root('../../doc')]).


