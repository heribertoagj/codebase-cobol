      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  00-ARGUMENTOS-ENTRADA.

       05  00-COLUNAS-DA-TABELA.
           10  00-CPROG                    PIC X(8).
           10  00-CJOB                     PIC X(8).
           10  00-QREG-COMIT               PIC S9(5)V COMP-3.
           10  00-CSIT-RESTT               PIC X(1).
           10  00-CUSUAR-SENHA             PIC X(7).
           10  00-HATULZ                   PIC X(26).
           10  00-WCHAVE-RESTT.
              49  00-WCHAVE-RESTT-LEN      PIC S9(4) COMP.
              49  00-WCHAVE-RESTT-TEXT     PIC X(250).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  00-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  00-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  00-RETORNO.
       05  00-COD-RETORNO                         PIC 9(04).
       05  00-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  00-PROXIMO-RESTART                     PIC 9(05).
       05  00-QTDE-RETORNADA                      PIC 9(03).
       05  00-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  00-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T00.
           10  00RT-CPROG                  PIC X(8).
           10  00RT-CJOB                   PIC X(8).
           10  00RT-QREG-COMIT             PIC S9(5)V COMP-3.
           10  00RT-CSIT-RESTT             PIC X(1).
           10  00RT-CUSUAR-SENHA           PIC X(7).
           10  00RT-HATULZ                 PIC X(26).
           10  00RT-WCHAVE-RESTT.
              49  00RT-WCHAVE-RESTT-LEN    PIC S9(4) COMP.
              49  00RT-WCHAVE-RESTT-TEXT   PIC X(250).
