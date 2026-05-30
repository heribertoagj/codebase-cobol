      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  87-ARGUMENTOS-ENTRADA.

       05  87-COLUNAS-DA-TABELA.
           10  87-CCONTR-LIM-DESC          PIC S9(9)V COMP-3.
           10  87-CVRSAO-CONTR-LIM         PIC S9(3)V COMP-3.
           10  87-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  87-HWORKF-CONTR-LIM         PIC X(26).
           10  87-CSIT-INIC-DESC           PIC S9(3)V COMP-3.
           10  87-HSIT-INIC-DESC           PIC X(26).
           10  87-CSIT-FNAL-DESC           PIC S9(3)V COMP-3.
           10  87-HSIT-FNAL-DESC           PIC X(26).
           10  87-RWORKF-CONTR-LIM.
              49  87-RWORKF-CONTR-LIM-LEN  PIC S9(4) COMP.
              49  87-RWORKF-CONTR-LIM-TEXT PIC X(240).
           10  87-HULT-ATULZ               PIC X(26).
           10  87-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  87-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  87-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  87-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  87-RETORNO.
       05  87-COD-RETORNO                         PIC 9(04).
       05  87-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  87-PROXIMO-RESTART                     PIC 9(05).
       05  87-QTDE-RETORNADA                      PIC 9(03).
       05  87-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  87-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T87.
           10  87RT-CCONTR-LIM-DESC          PIC S9(9)V COMP-3.
           10  87RT-CVRSAO-CONTR-LIM         PIC S9(3)V COMP-3.
           10  87RT-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  87RT-HWORKF-CONTR-LIM         PIC X(26).
           10  87RT-CSIT-INIC-DESC           PIC S9(3)V COMP-3.
           10  87RT-HSIT-INIC-DESC           PIC X(26).
           10  87RT-CSIT-FNAL-DESC           PIC S9(3)V COMP-3.
           10  87RT-HSIT-FNAL-DESC           PIC X(26).
           10  87RT-RWORKF-CONTR-LIM.
              49  87RT-RWORKF-CONTR-LIM-LEN  PIC S9(4) COMP.
              49  87RT-RWORKF-CONTR-LIM-TEXT PIC X(240).
           10  87RT-HULT-ATULZ               PIC X(26).
           10  87RT-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  87RT-CTERM                    PIC X(8).
