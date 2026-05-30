      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW512                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3512             *
      * DATA      : 10/06/2014                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01468 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW512-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW512-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW512-E-CMNTR-MSGRA     :   CODIGO DA MENSAGEM EXIBIDA      *
      * BVVEW512-E-NU-CTRL-EMIS    :   NUMERO DE CONTROLE EMISSOR      *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW512-REGISTRO.
               10 BVVEW512-BLOCO-ENTRADA.
                   15 BVVEW512-E-CHAMADOR          PIC  X(008).
                   15 BVVEW512-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW512-E-CMNTR-MSGRA       PIC  X(023).
                   15 BVVEW512-E-NU-CTRL-EMIS      PIC  X(023).
HSM                15 BVVEW512-E-DATA-CONSULTA     PIC  X(010).
               10 BVVEW512-BLOCO-RESTART.
                   15 BVVEW512-CMNTRC-REST-INI     PIC  X(023).
                   15 BVVEW512-CMNTRC-REST-FIM     PIC  X(023).
               10 BVVEW512-BLOCO-SAIDA.
                   15 BVVEW512-S-CMNTRC-MSGRA      PIC  X(023)
                                                   OCCURS 12 TIMES.
               10 BVVEW512-BLOCO-PERSISTENCIA.
                   15 BVVEW512-AREA                PIC  X(1000).
               10 BVVEW512-BLOCO-RETORNO.
                   15 BVVEW512-COD-RET             PIC  9(002).
                   15 BVVEW512-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
