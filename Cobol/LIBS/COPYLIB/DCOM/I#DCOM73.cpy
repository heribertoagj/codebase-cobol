      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB073 - DB2PRD.TSPROD_TX_REDE                         *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6865 - SELECT         - DCOM6866 - FETCH             *
      *    - DCOM6867 - INSERT         - DCOM6868 - UPDATE            *
      *    - DCOM6869 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  73-ARGUMENTOS-ENTRADA.

       05  73-COLUNAS-DA-TABELA.
           10  73-CPRODT                   PIC S9(3)V COMP-3.
           10  73-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  73-CINDCD-PSSOA-TOMAD       PIC X(1).
           10  73-CSGMTO-CLI               PIC S9(3)V COMP-3.
           10  73-CRTING                   PIC X(3).
DTS001     10  73-NNVEL-RTING              PIC S9(2)V COMP-3.
DTS001     10  73-HINIC-VGCIA-NVEL         PIC X(26).
           10  73-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  73-DINIC-TX-JURO            PIC X(10).
           10  73-NSEQ-BANDA-TX-REDE       PIC S9(5)V COMP-3.
           10  73-DFIM-TX-JURO             PIC X(10).
           10  73-CINDCD-TX-VLR-MIN        PIC X(2).
           10  73-VMIN-FAIXA-OPER          PIC S9(15)V9(2) COMP-3.
           10  73-CINDCD-TX-VLR-MAX        PIC X(2).
           10  73-VMAX-FAIXA-OPER          PIC S9(15)V9(2) COMP-3.
           10  73-CINDCD-TX-PRZ-MIN        PIC X(2).
           10  73-TMIN-OPER-DESC           PIC S9(5)V COMP-3.
           10  73-CINDCD-TX-PRZ-MAX        PIC X(2).
           10  73-TMAX-OPER-DESC           PIC S9(5)V COMP-3.
           10  73-PMIN-TX-REDE-DESC        PIC S9(3)V9(3) COMP-3.
           10  73-PMAX-TX-REDE-DESC        PIC S9(3)V9(3) COMP-3.
           10  73-CINDCD-FREQ-TX           PIC X(2).
           10  73-HULT-ATULZ               PIC X(26).
           10  73-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  73-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  73-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  73-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  73-RETORNO.
       05  73-COD-RETORNO                         PIC 9(04).
       05  73-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  73-PROXIMO-RESTART                     PIC 9(05).
       05  73-QTDE-RETORNADA                      PIC 9(03).
       05  73-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  73-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T73.
           10  73RT-CPRODT                 PIC S9(3)V COMP-3.
           10  73RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  73RT-CINDCD-PSSOA-TOMAD     PIC X(1).
           10  73RT-CSGMTO-CLI             PIC S9(3)V COMP-3.
           10  73RT-CRTING                 PIC X(3).
DTS001     10  73RT-NNVEL-RTING            PIC S9(2)V COMP-3.
DTS001     10  73RT-HINIC-VGCIA-NVEL       PIC X(26).
           10  73RT-CMEIO-ENTRD-DESC       PIC S9(3)V COMP-3.
           10  73RT-DINIC-TX-JURO          PIC X(10).
           10  73RT-NSEQ-BANDA-TX-REDE     PIC S9(5)V COMP-3.
           10  73RT-DFIM-TX-JURO           PIC X(10).
           10  73RT-CINDCD-TX-VLR-MIN      PIC X(2).
           10  73RT-VMIN-FAIXA-OPER        PIC S9(15)V9(2) COMP-3.
           10  73RT-CINDCD-TX-VLR-MAX      PIC X(2).
           10  73RT-VMAX-FAIXA-OPER        PIC S9(15)V9(2) COMP-3.
           10  73RT-CINDCD-TX-PRZ-MIN      PIC X(2).
           10  73RT-TMIN-OPER-DESC         PIC S9(5)V COMP-3.
           10  73RT-CINDCD-TX-PRZ-MAX      PIC X(2).
           10  73RT-TMAX-OPER-DESC         PIC S9(5)V COMP-3.
           10  73RT-PMIN-TX-REDE-DESC      PIC S9(3)V9(3) COMP-3.
           10  73RT-PMAX-TX-REDE-DESC      PIC S9(3)V9(3) COMP-3.
           10  73RT-CINDCD-FREQ-TX         PIC X(2).
           10  73RT-HULT-ATULZ             PIC X(26).
           10  73RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  73RT-CTERM                  PIC X(8).
