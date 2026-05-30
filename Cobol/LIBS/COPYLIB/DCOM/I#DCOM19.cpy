      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB019 - DB2PRD.TCONVE_TX_REDE                         *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6595 - SELECT         - DCOM6596 - FETCH             *
      *    - DCOM6597 - INSERT         - DCOM6598 - UPDATE            *
      *    - DCOM6599 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  19-ARGUMENTOS-ENTRADA.

       05  19-COLUNAS-DA-TABELA.
           10  19-CCONVE-GRAL-DESC         PIC S9(9)V COMP-3.
           10  19-CVRSAO-CONVE-GRAL        PIC S9(3)V COMP-3.
           10  19-CINDCD-PSSOA-TOMAD       PIC X(1).
           10  19-CSGMTO-CLI               PIC S9(3)V COMP-3.
           10  19-CRTING                   PIC X(3).
DTS001     10  19-NNVEL-RTING              PIC S9(2)V COMP-3.
DTS001     10  19-HINIC-VGCIA-NVEL         PIC X(26).
           10  19-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  19-DINIC-TX-JURO            PIC X(10).
           10  19-NSEQ-BANDA-TX-REDE       PIC S9(5)V COMP-3.
           10  19-DFIM-TX-JURO             PIC X(10).
           10  19-CINDCD-TX-VLR-MIN        PIC X(2).
           10  19-VMIN-FAIXA-OPER          PIC S9(15)V9(2) COMP-3.
           10  19-CINDCD-TX-VLR-MAX        PIC X(2).
           10  19-VMAX-FAIXA-OPER          PIC S9(15)V9(2) COMP-3.
           10  19-CINDCD-TX-PRZ-MIN        PIC X(2).
           10  19-TMIN-OPER-DESC           PIC S9(5)V COMP-3.
           10  19-CINDCD-TX-PRZ-MAX        PIC X(2).
           10  19-TMAX-OPER-DESC           PIC S9(5)V COMP-3.
           10  19-PMIN-TX-REDE-DESC        PIC S9(3)V9(3) COMP-3.
           10  19-PMAX-TX-REDE-DESC        PIC S9(3)V9(3) COMP-3.
           10  19-CINDCD-FREQ-TX           PIC X(2).
           10  19-HULT-ATULZ               PIC X(26).
           10  19-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  19-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  19-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  19-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  19-RETORNO.
       05  19-COD-RETORNO                         PIC 9(04).
       05  19-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  19-PROXIMO-RESTART                     PIC 9(05).
       05  19-QTDE-RETORNADA                      PIC 9(03).
       05  19-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  19-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T19.
           10  19RT-CCONVE-GRAL-DESC       PIC S9(9)V COMP-3.
           10  19RT-CVRSAO-CONVE-GRAL      PIC S9(3)V COMP-3.
           10  19RT-CINDCD-PSSOA-TOMAD     PIC X(1).
           10  19RT-CSGMTO-CLI             PIC S9(3)V COMP-3.
           10  19RT-CRTING                 PIC  X(3).
DTS001     10  19RT-NNVEL-RTING            PIC S9(2)V COMP-3.
DTS001     10  19RT-HINIC-VGCIA-NVEL       PIC X(26).
           10  19RT-CMEIO-ENTRD-DESC       PIC S9(3)V COMP-3.
           10  19RT-DINIC-TX-JURO          PIC X(10).
           10  19RT-NSEQ-BANDA-TX-REDE     PIC S9(5)V COMP-3.
           10  19RT-DFIM-TX-JURO           PIC X(10).
           10  19RT-CINDCD-TX-VLR-MIN      PIC X(2).
           10  19RT-VMIN-FAIXA-OPER        PIC S9(15)V9(2) COMP-3.
           10  19RT-CINDCD-TX-VLR-MAX      PIC X(2).
           10  19RT-VMAX-FAIXA-OPER        PIC S9(15)V9(2) COMP-3.
           10  19RT-CINDCD-TX-PRZ-MIN      PIC X(2).
           10  19RT-TMIN-OPER-DESC         PIC S9(5)V COMP-3.
           10  19RT-CINDCD-TX-PRZ-MAX      PIC X(2).
           10  19RT-TMAX-OPER-DESC         PIC S9(5)V COMP-3.
           10  19RT-PMIN-TX-REDE-DESC      PIC S9(3)V9(3) COMP-3.
           10  19RT-PMAX-TX-REDE-DESC      PIC S9(3)V9(3) COMP-3.
           10  19RT-CINDCD-FREQ-TX         PIC X(2).
           10  19RT-HULT-ATULZ             PIC X(26).
           10  19RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  19RT-CTERM                  PIC X(8).
