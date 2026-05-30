      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW515                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3515             *
      * DATA      : 10/09/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01137 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW515-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW515-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW515-E-CMNTR-MSGRA     :   CHAVE DA MENSAGEM               *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW515-REGISTRO.
               10 BVVEW515-BLOCO-ENTRADA.
                   15 BVVEW515-E-CHAMADOR          PIC  X(008).
                   15 BVVEW515-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW515-E-CMNTR-MSGRA       PIC  X(023).
HSM                15 BVVEW515-E-DATA-CONSULTA     PIC  X(010).
               10 BVVEW515-BLOCO-RESTART.
                   15 BVVEW515-CHAVE-INI           PIC  9(003).
                   15 BVVEW515-CHAVE-FIM           PIC  9(003).
                   15 BVVEW515-POSICAO-REST-INI    PIC  9(004).
                   15 BVVEW515-POSICAO-REST-FIM    PIC  9(004).
               10 BVVEW515-BLOCO-PERSISTENCIA.
                   15 BVVEW515-AREA                PIC  X(1000).
               10 BVVEW515-BLOCO-RETORNO.
                   15 BVVEW515-COD-RET             PIC  9(002).
                   15 BVVEW515-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
