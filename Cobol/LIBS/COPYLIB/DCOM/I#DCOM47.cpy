      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  47-ARGUMENTOS-ENTRADA.

       05  47-COLUNAS-DA-TABELA.
           10  47-CPRODT                   PIC S9(03)V COMP-3.
           10  47-IPRODT                   PIC  X(60).
           10  47-IABREV-PRODT             PIC  X(12).
           10  47-CSGL-PRODT-DESC          PIC  X(06).
           10  47-CIDTFD-TX-MIN-JURO       PIC S9(05)V COMP-3.
           10  47-CIDTFD-TX-MAX-JURO       PIC S9(05)V COMP-3.
           10  47-HULT-ATULZ               PIC  X(26).
           10  47-CFUNC-BDSCO              PIC S9(09)V COMP-3.
           10  47-CTERM                    PIC  X(08).
           10  47-CIDTFD-TX-CUSTO          PIC S9(05)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  47-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  47-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  47-RETORNO.
       05  47-COD-RETORNO                         PIC 9(04).
       05  47-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  47-PROXIMO-RESTART                     PIC 9(05).
       05  47-QTDE-RETORNADA                      PIC 9(03).
       05  47-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  47-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T47.
           10  47RT-CPRODT                 PIC S9(03)V COMP-3.
           10  47RT-IPRODT                 PIC  X(60).
           10  47RT-IABREV-PRODT           PIC  X(12).
           10  47RT-CSGL-PRODT-DESC        PIC  X(06).
           10  47RT-CIDTFD-TX-MIN-JURO     PIC S9(05)V COMP-3.
           10  47RT-CIDTFD-TX-MAX-JURO     PIC S9(05)V COMP-3.
           10  47RT-HULT-ATULZ             PIC  X(26).
           10  47RT-CFUNC-BDSCO            PIC S9(09)V COMP-3.
           10  47RT-CTERM                  PIC  X(08).
           10  47RT-CIDTFD-TX-CUSTO        PIC S9(05)V COMP-3.
