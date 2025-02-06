# Generated from AgentSpeak.g4 by ANTLR 4.9
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .AgentSpeakParser import AgentSpeakParser
else:
    from AgentSpeakParser import AgentSpeakParser

# This class defines a complete listener for a parse tree produced by AgentSpeakParser.
class AgentSpeakListener(ParseTreeListener):

    # Enter a parse tree produced by AgentSpeakParser#agent.
    def enterAgent(self, ctx:AgentSpeakParser.AgentContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#agent.
    def exitAgent(self, ctx:AgentSpeakParser.AgentContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#init_bels.
    def enterInit_bels(self, ctx:AgentSpeakParser.Init_belsContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#init_bels.
    def exitInit_bels(self, ctx:AgentSpeakParser.Init_belsContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#beliefs.
    def enterBeliefs(self, ctx:AgentSpeakParser.BeliefsContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#beliefs.
    def exitBeliefs(self, ctx:AgentSpeakParser.BeliefsContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#rules.
    def enterRules(self, ctx:AgentSpeakParser.RulesContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#rules.
    def exitRules(self, ctx:AgentSpeakParser.RulesContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#init_goal.
    def enterInit_goal(self, ctx:AgentSpeakParser.Init_goalContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#init_goal.
    def exitInit_goal(self, ctx:AgentSpeakParser.Init_goalContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#plan.
    def enterPlan(self, ctx:AgentSpeakParser.PlanContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#plan.
    def exitPlan(self, ctx:AgentSpeakParser.PlanContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#triggering_event.
    def enterTriggering_event(self, ctx:AgentSpeakParser.Triggering_eventContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#triggering_event.
    def exitTriggering_event(self, ctx:AgentSpeakParser.Triggering_eventContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#literal.
    def enterLiteral(self, ctx:AgentSpeakParser.LiteralContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#literal.
    def exitLiteral(self, ctx:AgentSpeakParser.LiteralContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#context.
    def enterContext(self, ctx:AgentSpeakParser.ContextContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#context.
    def exitContext(self, ctx:AgentSpeakParser.ContextContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#log_expr.
    def enterLog_expr(self, ctx:AgentSpeakParser.Log_exprContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#log_expr.
    def exitLog_expr(self, ctx:AgentSpeakParser.Log_exprContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#simple_log_expr.
    def enterSimple_log_expr(self, ctx:AgentSpeakParser.Simple_log_exprContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#simple_log_expr.
    def exitSimple_log_expr(self, ctx:AgentSpeakParser.Simple_log_exprContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#body.
    def enterBody(self, ctx:AgentSpeakParser.BodyContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#body.
    def exitBody(self, ctx:AgentSpeakParser.BodyContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#body_formula.
    def enterBody_formula(self, ctx:AgentSpeakParser.Body_formulaContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#body_formula.
    def exitBody_formula(self, ctx:AgentSpeakParser.Body_formulaContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#atomic_formula.
    def enterAtomic_formula(self, ctx:AgentSpeakParser.Atomic_formulaContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#atomic_formula.
    def exitAtomic_formula(self, ctx:AgentSpeakParser.Atomic_formulaContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#list_of_terms.
    def enterList_of_terms(self, ctx:AgentSpeakParser.List_of_termsContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#list_of_terms.
    def exitList_of_terms(self, ctx:AgentSpeakParser.List_of_termsContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#term.
    def enterTerm(self, ctx:AgentSpeakParser.TermContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#term.
    def exitTerm(self, ctx:AgentSpeakParser.TermContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#term_value.
    def enterTerm_value(self, ctx:AgentSpeakParser.Term_valueContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#term_value.
    def exitTerm_value(self, ctx:AgentSpeakParser.Term_valueContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#internal_action.
    def enterInternal_action(self, ctx:AgentSpeakParser.Internal_actionContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#internal_action.
    def exitInternal_action(self, ctx:AgentSpeakParser.Internal_actionContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#list_structure.
    def enterList_structure(self, ctx:AgentSpeakParser.List_structureContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#list_structure.
    def exitList_structure(self, ctx:AgentSpeakParser.List_structureContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#rel_expr.
    def enterRel_expr(self, ctx:AgentSpeakParser.Rel_exprContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#rel_expr.
    def exitRel_expr(self, ctx:AgentSpeakParser.Rel_exprContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#rel_term.
    def enterRel_term(self, ctx:AgentSpeakParser.Rel_termContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#rel_term.
    def exitRel_term(self, ctx:AgentSpeakParser.Rel_termContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#arithm_expr.
    def enterArithm_expr(self, ctx:AgentSpeakParser.Arithm_exprContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#arithm_expr.
    def exitArithm_expr(self, ctx:AgentSpeakParser.Arithm_exprContext):
        pass


    # Enter a parse tree produced by AgentSpeakParser#arithm_term.
    def enterArithm_term(self, ctx:AgentSpeakParser.Arithm_termContext):
        pass

    # Exit a parse tree produced by AgentSpeakParser#arithm_term.
    def exitArithm_term(self, ctx:AgentSpeakParser.Arithm_termContext):
        pass



del AgentSpeakParser