      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB014 - DB2PRD.TCONVE_CLI_TX_REDE                     *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6570 - SELECT         - DCOM6571 - FETCH             *
      *    - DCOM6572 - INSERT         - DCOM6573 - UPDATE            *
      *    - DCOM6574 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  14-ARGUMENTOS-ENTRADA.

       05  14-COLUNAS-DA-TABELA.
           10  14-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  14-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
           10  14-CINDCD-PSSOA-TOMAD       PIC X(1).
           10  14-CSGMTO-CLI               PIC S9(3)V COMP-3.
           10  14-CRTING                   PIC X(3).
DTS001     10  14-NNVEL-RTING              PIC S9(2)V COMP-3.
DTS001     10  14-HINIC-VGCIA-NVEL         PIC X(26).
           10  14-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  14-DINIC-TX-JURO            PIC X(10).
           10  14-NSEQ-BANDA-TX-REDE       PIC S9(5)V COMP-3.
           10  14-DFIM-TX-JURO             PIC X(10).
           10  14-CINDCD-TX-VLR-MIN        PIC X(2).
           10  14-VMIN-FAIXA-OPER          PIC S9(15)V9(2) COMP-3.
           10  14-CINDCD-TX-VLR-MAX        PIC X(2).
           10  14-VMAX-FAIXA-OPER          PIC S9(15)V9(2) COMP-3.
           10  14-CINDCD-TX-PRZ-MIN        PIC X(2).
           10  14-TMIN-OPER-DESC           PIC S9(5)V COMP-3.
           10  14-CINDCD-TX-PRZ-MAX        PIC X(2).
           10  14-TMAX-OPER-DESC           PIC S9(5)V COMP-3.
           10  14-PMIN-TX-REDE-DESC        PIC S9(3)V9(3) COMP-3.
           10  14-PMAX-TX-REDE-DESC        PIC S9(3)V9(3) COMP-3.
           10  14-CINDCD-FREQ-TX           PIC X(2).
           10  14-HULT-ATULZ               PIC X(26).
           10  14-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  14-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  14-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  14-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  14-RETORNO.
       05  14-COD-RETORNO                         PIC 9(04).
       05  14-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  14-PROXIMO-RESTART                     PIC 9(05).
       05  14-QTDE-RETORNADA                      PIC 9(03).
       05  14-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  14-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T14.
           10  14RT-CCONVE-CLI-DESC        PIC S9(9)V COMP-3.
           10  14RT-CVRSAO-CONVE-CLI       PIC S9(3)V COMP-3.
           10  14RT-CINDCD-PSSOA-TOMAD     PIC X(1).
           10  14RT-CSGMTO-CLI             PIC S9(3)V COMP-3.
           10  14RT-CRTING                 PIC X(3).
DTS001     10  14RT-NNVEL-RTING            PIC S9(2)V COMP-3.
DTS001     10  14RT-HINIC-VGCIA-NVEL       PIC X(26).
           10  14RT-CMEIO-ENTRD-DESC       PIC S9(3)V COMP-3.
           10  14RT-DINIC-TX-JURO          PIC X(10).
           10  14RT-NSEQ-BANDA-TX-REDE     PIC S9(5)V COMP-3.
           10  14RT-DFIM-TX-JURO           PIC X(10).
           10  14RT-CINDCD-TX-VLR-MIN      PIC X(2).
           10  14RT-VMIN-FAIXA-OPER        PIC S9(15)V9(2) COMP-3.
           10  14RT-CINDCD-TX-VLR-MAX      PIC X(2).
           10  14RT-VMAX-FAIXA-OPER        PIC S9(15)V9(2) COMP-3.
           10  14RT-CINDCD-TX-PRZ-MIN      PIC X(2).
           10  14RT-TMIN-OPER-DESC         PIC S9(5)V COMP-3.
           10  14RT-CINDCD-TX-PRZ-MAX      PIC X(2).
           10  14RT-TMAX-OPER-DESC         PIC S9(5)V COMP-3.
           10  14RT-PMIN-TX-REDE-DESC      PIC S9(3)V9(3) COMP-3.
           10  14RT-PMAX-TX-REDE-DESC      PIC S9(3)V9(3) COMP-3.
           10  14RT-CINDCD-FREQ-TX         PIC X(2).
           10  14RT-HULT-ATULZ             PIC X(26).
           10  14RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  14RT-CTERM                  PIC X(8).
