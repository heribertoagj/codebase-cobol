      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  88-ARGUMENTOS-ENTRADA.

       05  88-COLUNAS-DA-TABELA.
           10  88-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  88-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
FVB   *    10  88-CEVNTO-WORKF-DESC        PIC S9(3)V COMP-3.
FVB        10  88-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  88-HWORKF-CONVE-CLI         PIC X(10).
FVB        10  88-CSIT-INIC-DESC           PIC S9(3)V COMP-3.
FVB        10  88-HSIT-INIC-DESC           PIC X(26).
FVB        10  88-CSIT-FNAL-DESC           PIC S9(3)V COMP-3.
FVB        10  88-HSIT-FNAL-DESC           PIC X(26).
           10  88-RWORKF-CONVE-CLI.
              49  88-RWORKF-CONVE-CLI-LEN  PIC S9(4) COMP.
              49  88-RWORKF-CONVE-CLI-TEXT PIC X(240).
           10  88-HULT-ATULZ               PIC X(26).
           10  88-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  88-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  88-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  88-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  88-RETORNO.
       05  88-COD-RETORNO                         PIC 9(04).
       05  88-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  88-PROXIMO-RESTART                     PIC 9(05).
       05  88-QTDE-RETORNADA                      PIC 9(03).
       05  88-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  88-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T88.
           10  88RT-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  88RT-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
FVB   *    10  88RT-CEVNTO-WORKF-DESC        PIC S9(3)V COMP-3.
FVB        10  88RT-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  88RT-HWORKF-CONVE-CLI         PIC X(10).
FVB        10  88RT-CSIT-INIC-DESC           PIC S9(3)V COMP-3.
FVB        10  88RT-HSIT-INIC-DESC           PIC X(26).
FVB        10  88RT-CSIT-FNAL-DESC           PIC S9(3)V COMP-3.
FVB        10  88RT-HSIT-FNAL-DESC           PIC X(26).
           10  88RT-RWORKF-CONVE-CLI.
              49  88RT-RWORKF-CONVE-CLI-LEN  PIC S9(4) COMP.
              49  88RT-RWORKF-CONVE-CLI-TEXT PIC X(240).
           10  88RT-HULT-ATULZ               PIC X(26).
           10  88RT-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  88RT-CTERM                    PIC X(8).
