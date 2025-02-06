import os
import platform
from dataclasses import dataclass

from .mas2j.MAS2JavaListener import MAS2JavaListener
from .mas2j.MAS2JavaParser import MAS2JavaParser


@dataclass
class Agent:
    name: str
    filename: str
    count: int
    options: str


class Mas2fpGenerator(MAS2JavaListener):
    def __init__(self, output_path: str) -> None:
        super().__init__()

        self._output = ""
        self._name = ""
        self._env_name = None
        self._agents = []
        self._output_path = output_path

    @property
    def output(self) -> str:
        return self._output

    @property
    def env_name(self) -> str:
        return self._env_name

    @property
    def agents(self) -> list[Agent]:
        return self._agents

    def enterMas(self, ctx:MAS2JavaParser.MasContext):
        self._name = ctx.ID().getText()

    def enterAgent(self, ctx:MAS2JavaParser.AgentContext):
        agent_name = ctx.ID().getText()
        agent_count = ctx.NUMBER()
        agent_count = 1 if agent_count is None else int(ctx.NUMBER().getText())

        agent_filename = ctx.FILENAME()
        if agent_filename is None:
            agent_filename = agent_name + ".fap"
        else:
            agent_filename = agent_filename.getText().replace(".asl", ".fap")

        agt_options = ctx.agt_options()
        options = agt_options.getText() if agt_options else "[(debug,systemdbg)]"

        agent = Agent(agent_name, agent_filename, agent_count, options)
        self.agents.append(agent)

        agent_path_without_extension = os.path.join(self._output_path, agent.filename.replace(".fap", ""))

        if platform.system() == "Windows":
            agent_path_without_extension = os.path.normpath(agent_path_without_extension).replace('\\', '/')

        self._output += f'load("{agent.name}","{agent_path_without_extension}",{agent.count},{agent.options}).\n'

    def enterInfrastructure(self, ctx:MAS2JavaParser.InfrastructureContext):
        infrastructure = ctx.ID().symbol.text
        if infrastructure != "Centralised":
            raise Exception("Only Centralised infrastructure is supported")

    def enterEnvironment(self, ctx:MAS2JavaParser.EnvironmentContext):
        env_path = ctx.STRING().getText().strip('"')
        self._output += f'include_environment("{env_path}").\n'

        self._output += "\n"

        env_name = ctx.ID().getText()
        parameters = "[]" if ctx.parameters() is None else ctx.parameters().getText()

        self._output += f'set_environment({env_name}, {parameters}).\n'

        self._env_name = env_name

        self._output += "\n"

    def enterAgent_defaults(self, ctx:MAS2JavaParser.Agent_defaultsContext):
        parameters = ctx.parameters().getText()
        if not parameters:
            raise ValueError("Agent defaults must have parameters")

        self._output += f'set_agents({parameters}).\n'

        self._output += "\n"

    def enterExec_control(self, ctx:MAS2JavaParser.Exec_controlContext):
        raise Exception("Exec_control is not supported")

    def enterAgt_arch_class(self, ctx:MAS2JavaParser.Agt_arch_classContext):
        raise Exception("agentArchClass is not supported")

    def enterAgt_at(self, ctx:MAS2JavaParser.Agt_atContext):
        raise Exception("at option is not supported")

    def enterAgt_class(self, ctx:MAS2JavaParser.Agt_classContext):
        raise Exception("agentClass is not supported")
