      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C3 - DB2PRD.TMOVTC_DESC_COML       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  C3-ARGUMENTOS-ENTRADA.

       05  C3-COLUNAS-DA-TABELA.
           10  C3-CELMTO-DESC-COML             PIC S9(3)V COMP-3.
           10  C3-CEVNTO-DESC-COML             PIC S9(3)V COMP-3.
           10  C3-HULT-ATULZ                   PIC X(26).
           10  C3-CINDCD-MANUT-REG             PIC X(1).
           10  C3-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  C3-CCHAVE-ELMTO-DESC            PIC X(45).
           10  C3-WREG-MOVTC-DESC.
              49  C3-WREG-MOVTC-DESC-LEN       PIC S9(4) COMP.
              49  C3-WREG-MOVTC-DESC-TEXT      PIC X(1500).
           10  C3-CTRANS-PROG-ATULZ            PIC X(8).
           10  C3-CINDCD-MODLD-PROCM           PIC X(1).
           10  C3-CEMPR-INC                    PIC S9(5)V COMP-3.
           10  C3-CDEPDC                       PIC S9(5)V COMP-3.
           10  C3-DMOVTC-DESC-COML             PIC X(10).
           10  C3-CCATLG-COMPS-REG             PIC X(8).
           10  C3-NSEQ-PROCM-EVNTO             PIC S9(3)V COMP-3.
           10  C3-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C3-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C3-RETORNO.
       05  C3-COD-RETORNO                         PIC 9(04).
       05  C3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C3-PROXIMO-RESTART                     PIC 9(05).
       05  C3-QTDE-RETORNADA                      PIC 9(03).
       05  C3-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC3.
           10  C3RT-CELMTO-DESC-COML           PIC S9(3)V COMP-3.
           10  C3RT-CEVNTO-DESC-COML           PIC S9(3)V COMP-3.
           10  C3RT-HULT-ATULZ                 PIC X(26).
           10  C3RT-CINDCD-MANUT-REG           PIC X(1).
           10  C3RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  C3RT-CCHAVE-ELMTO-DESC          PIC X(45).
           10  C3RT-WREG-MOVTC-DESC.
              49  C3RT-WREG-MOVTC-DESC-LEN     PIC S9(4) COMP.
              49  C3RT-WREG-MOVTC-DESC-TEXT    PIC X(1500).
           10  C3RT-CTRANS-PROG-ATULZ          PIC X(8).
           10  C3RT-CINDCD-MODLD-PROCM         PIC X(1).
           10  C3RT-CEMPR-INC                  PIC S9(5)V COMP-3.
           10  C3RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  C3RT-DMOVTC-DESC-COML           PIC X(10).
           10  C3RT-CCATLG-COMPS-REG           PIC X(8).
           10  C3RT-NSEQ-PROCM-EVNTO           PIC S9(3)V COMP-3.
           10  C3RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C3RT-CTERM                      PIC X(8).
