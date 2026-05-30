      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW780                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3780             *
      * DATA      : 10/19/2020                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01380 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW780-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW780-E-OPCAO-PESQ      :   OPCAO DE PESQUISA               *
      *                                1 - DO DIA                      *
      *                                2 - DO HISTORICO                *
      * BVVEW780-E-NMNTRC-TRFGO-ARQ:   CODIGO DO ARQUIVO EXIBIDO       *
      * BVVEW780-E-NU-CTRL-EMIS    :   NUMERO DE CONTROLE EMISSOR      *
DT1311* BVVEW780-E-IARQ-MNTRC      :   NOME DO ARQUIVO                 *
DT1311* BVVEW780-E-CSNLZD-MSGRA    :   SITUACAO DO ARQUIVO             *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW780-REGISTRO.
               10 BVVEW780-BLOCO-ENTRADA.
                   15 BVVEW780-E-CHAMADOR          PIC  X(008).
                   15 BVVEW780-E-OPCAO-PESQ        PIC  9(001).
                   15 BVVEW780-E-NMNTRC-TRFGO-ARQ  PIC  9(018).
                   15 BVVEW780-E-CCTRL-RQUIS-ORIGN PIC  X(020).
DT1311             15 BVVEW780-E-IARQ-MNTRC        PIC  X(080).
DT1311             15 BVVEW780-E-CSNLZD-MSGRA      PIC  9(003).
               10 BVVEW780-BLOCO-RESTART.
                   15 BVVEW780-CMNTRC-REST-INI     PIC  9(018).
                   15 BVVEW780-CMNTRC-REST-FIM     PIC  9(018).
               10 BVVEW780-BLOCO-SAIDA.
                   15 BVVEW780-S-NMNTRC-TRFGO-ARQ  PIC  9(018)
                                                   OCCURS 12 TIMES.
               10 BVVEW780-BLOCO-PERSISTENCIA.
                   15 BVVEW780-AREA                PIC  X(1000).
               10 BVVEW780-BLOCO-RETORNO.
                   15 BVVEW780-COD-RET             PIC  9(002).
                   15 BVVEW780-MSG-RET             PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
