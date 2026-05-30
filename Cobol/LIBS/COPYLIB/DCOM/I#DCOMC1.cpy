      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C1 - DB2PRD.THIST_WORKF_OPER       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  C1-ARGUMENTOS-ENTRADA.

       05  C1-COLUNAS-DA-TABELA.
           10  C1-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C1-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  C1-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  C1-CEVNTO-DESC-COML             PIC S9(3)V COMP-3.
           10  C1-HWORKF-OPER-DESC             PIC X(26).
           10  C1-CSIT-INIC-DESC               PIC S9(3)V COMP-3.
           10  C1-HSIT-INIC-DESC               PIC X(26).
           10  C1-CSIT-FNAL-DESC               PIC S9(3)V COMP-3.
           10  C1-HSIT-FNAL-DESC               PIC X(26).
           10  C1-RWORKF-OPER-DESC.
              49  C1-RWORKF-OPER-DESC-LEN      PIC S9(4) COMP.
              49  C1-RWORKF-OPER-DESC-TEXT     PIC X(240).
           10  C1-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C1-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C1-RETORNO.
       05  C1-COD-RETORNO                         PIC 9(04).
       05  C1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C1-PROXIMO-RESTART                     PIC 9(05).
       05  C1-QTDE-RETORNADA                      PIC 9(03).
       05  C1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC1.
           10  C1RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C1RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  C1RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  C1RT-CEVNTO-DESC-COML           PIC S9(3)V COMP-3.
           10  C1RT-HWORKF-OPER-DESC           PIC X(26).
           10  C1RT-CSIT-INIC-DESC             PIC S9(3)V COMP-3.
           10  C1RT-HSIT-INIC-DESC             PIC X(26).
           10  C1RT-CSIT-FNAL-DESC             PIC S9(3)V COMP-3.
           10  C1RT-HSIT-FNAL-DESC             PIC X(26).
           10  C1RT-RWORKF-OPER-DESC.
              49  C1RT-RWORKF-OPER-DESC-LEN    PIC S9(4) COMP.
              49  C1RT-RWORKF-OPER-DESC-TEXT   PIC X(240).
           10  C1RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C1RT-CTERM                      PIC X(8).
