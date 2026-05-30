      ******************************************************************
      * NOME BOOK.: I#BVVEMQ - AREA DO MODULO BVVE1360                 *
      * DESCRICAO.: CONSULTAR CREDENCIADORA ATIVA                      *
      * DATA......: MARCO/2021                                         *
      * AUTOR.....: FERNANDO LUIZ DE SANTI - WIPRO                     *
      ******************************************************************
       01  BVVEMQ-AREA-BVVE.
           05  BVVEMQ-ENT-ENTRADA.
               10  BVVEMQ-ENT-COD-PARM    PIC  9(003).
               10  BVVEMQ-ENT-RESERVA     PIC  X(067).
           05  BVVEMQ-SAI-SAIDA.
               10  BVVEMQ-SAI-COD-RETORNO PIC  9(002).
               10  BVVEMQ-SAI-CONTEUDO-PARM
                                          PIC  X(060).
               10  BVVEMQ-SAI-RETORNO.
                   15  BVVEMQ-SAI-MENSAGEM
                                          PIC  X(050).
                   15  BVVEMQ-SAI-COD-SQLCODE
                                          PIC S9(004).
                   15  BVVEMQ-SAI-TABELA
                                          PIC  X(008).
                   15  BVVEMQ-SAI-COMANDO
                                          PIC  X(010).
                   15  BVVEMQ-SAI-LOCAL
                                          PIC  9(002).

