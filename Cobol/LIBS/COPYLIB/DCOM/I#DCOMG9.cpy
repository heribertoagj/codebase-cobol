      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G9 - DB2PRD.TWORKF_OPER_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G9-ARGUMENTOS-ENTRADA.

       05  G9-COLUNAS-DA-TABELA.
           10  G9-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G9-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G9-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  G9-CEVNTO-DESC-COML             PIC S9(3)V COMP-3.
           10  G9-HWORKF-OPER-DESC             PIC X(26).
           10  G9-CSIT-INIC-DESC               PIC S9(3)V COMP-3.
           10  G9-HSIT-INIC-DESC               PIC X(26).
           10  G9-CSIT-FNAL-DESC               PIC S9(3)V COMP-3.
           10  G9-HSIT-FNAL-DESC               PIC X(26).
           10  G9-RWORKF-OPER-DESC.
              49  G9-RWORKF-OPER-DESC-LEN      PIC S9(4) COMP.
              49  G9-RWORKF-OPER-DESC-TEXT     PIC X(240).
           10  G9-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G9-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G9-RETORNO.
       05  G9-COD-RETORNO                         PIC 9(04).
       05  G9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G9-PROXIMO-RESTART                     PIC 9(05).
       05  G9-QTDE-RETORNADA                      PIC 9(03).
       05  G9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG9.
           10  G9RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G9RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G9RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  G9RT-CEVNTO-DESC-COML           PIC S9(3)V COMP-3.
           10  G9RT-HWORKF-OPER-DESC           PIC X(26).
           10  G9RT-CSIT-INIC-DESC             PIC S9(3)V COMP-3.
           10  G9RT-HSIT-INIC-DESC             PIC X(26).
           10  G9RT-CSIT-FNAL-DESC             PIC S9(3)V COMP-3.
           10  G9RT-HSIT-FNAL-DESC             PIC X(26).
           10  G9RT-RWORKF-OPER-DESC.
              49  G9RT-RWORKF-OPER-DESC-LEN    PIC S9(4) COMP.
              49  G9RT-RWORKF-OPER-DESC-TEXT   PIC X(240).
           10  G9RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G9RT-CTERM                      PIC X(8).
