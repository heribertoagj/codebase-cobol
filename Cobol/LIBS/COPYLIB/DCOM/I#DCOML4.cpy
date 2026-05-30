      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  L4-ARGUMENTOS-ENTRADA.

       05  L4-COLUNAS-DA-TABELA.
           10  L4-NSMULA-OPER-DESC      PIC S9(9)V COMP-3.
           10  L4-VCUSTO-DESC-DIA       PIC S9(8)V9(7) COMP-3.
           10  L4-VCUSTO-DESC-MES       PIC S9(8)V9(7) COMP-3.
           10  L4-VCUSTO-DESC-ANO       PIC S9(8)V9(7) COMP-3.
           10  L4-CFUNC-BDSCO           PIC S9(9)V COMP-3.
           10  L4-HATULZ                PIC X(26).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  L4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  L4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  L4-RETORNO.
       05  L4-COD-RETORNO                         PIC 9(04).
       05  L4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  L4-PROXIMO-RESTART                     PIC 9(05).
       05  L4-QTDE-RETORNADA                      PIC 9(03).
       05  L4-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  L4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TL4.
           10  L4RT-NSMULA-OPER-DESC      PIC S9(9)V COMP-3.
           10  L4RT-VCUSTO-DESC-DIA       PIC S9(8)V9(7) COMP-3.
           10  L4RT-VCUSTO-DESC-MES       PIC S9(8)V9(7) COMP-3.
           10  L4RT-VCUSTO-DESC-ANO       PIC S9(8)V9(7) COMP-3.
           10  L4RT-CFUNC-BDSCO           PIC S9(9)V COMP-3.
           10  L4RT-HATULZ                PIC X(26).
