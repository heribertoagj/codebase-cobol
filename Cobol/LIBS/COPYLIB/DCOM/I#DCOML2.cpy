      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  L2-ARGUMENTOS-ENTRADA.

       05  L2-COLUNAS-DA-TABELA.
           10  L2-DANO-OPER-DESC        PIC S9(4)V COMP-3.
           10  L2-NSEQ-OPER-DESC        PIC S9(9)V COMP-3.
           10  L2-HATULZ                PIC X(26).
           10  L2-VCUSTO-DESC-DIA       PIC S9(8)V9(7) COMP-3.
           10  L2-VCUSTO-DESC-MES       PIC S9(8)V9(7) COMP-3.
           10  L2-VCUSTO-DESC-ANO       PIC S9(8)V9(7) COMP-3.
           10  L2-CFUNC-BDSCO           PIC S9(9)V COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  L2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  L2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  L2-RETORNO.
       05  L2-COD-RETORNO                         PIC 9(04).
       05  L2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  L2-PROXIMO-RESTART                     PIC 9(05).
       05  L2-QTDE-RETORNADA                      PIC 9(03).
       05  L2-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  L2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TL2.
           10  L2RT-DANO-OPER-DESC        PIC S9(4)V COMP-3.
           10  L2RT-NSEQ-OPER-DESC        PIC S9(9)V COMP-3.
           10  L2RT-HATULZ                PIC X(26).
           10  L2RT-VCUSTO-DESC-DIA       PIC S9(8)V9(7) COMP-3.
           10  L2RT-VCUSTO-DESC-MES       PIC S9(8)V9(7) COMP-3.
           10  L2RT-VCUSTO-DESC-ANO       PIC S9(8)V9(7) COMP-3.
           10  L2RT-CFUNC-BDSCO           PIC S9(9)V COMP-3.
