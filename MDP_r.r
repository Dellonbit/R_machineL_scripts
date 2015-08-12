#mdp_example_forest generates a transition probability (SxSxA) array P and a reward (SxA) matrix R that model the following problem.
# A forest is managed by two actions: Wait and Cut. An action is decided each year with first the objective to maintain an old forest 
# for wildlife and second to make money selling cut wood. Each year there is a probability p that a fire burns the forest.
# Here is the modelisation of this problem. Let 1, ... S be the states of the forest. the Sth state being the oldest. 
# Let Wait be action 1 and Cut action 2. After a fire, the forest is in the youngest state, that is state 1
library (MDPtoolbox)
mdp <- mdp_example_forest(6, 4, 2, 0.1) 
# value iteration of forest example
valIter <- mdp_value_iteration(mdp$P, mdp$R, 0.3, .2, 2000)
valIter

#policy iteration of forest example 
polIter <- mdp_policy_iteration(mdp$P, mdp$R, 0.9)
polIter

## applying q-learning to forest problem 
mdpQue <- mdp_Q_learning(mdp$P, mdp$R, 0.9)
mdpQue




## examplee 2: random MDP
library (MDPtoolbox)
mdprnd <-mdp_example_rand(2, 2, FALSE, matrix(c(1,0,1,1),2,2))
valIterrnd <- mdp_value_iteration(mdprnd$P, mdprnd$R, 0.3, .2, 2000)
valIterrnd

#policy iteration of forest example 
polIterrnd <- mdp_policy_iteration(mdprnd$P, mdprnd$R, 0.9)
polIterrnd

## applying q-learning to forest problem 
mdpQuernd <- mdp_Q_learning(mdprnd$P, mdprnd$R, 0.1)
mdpQuernd

