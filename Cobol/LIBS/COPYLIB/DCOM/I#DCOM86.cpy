      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  86-ARGUMENTOS-ENTRADA.

       05  86-COLUNAS-DA-TABELA.
           10  86-CTPO-TX-JURO-DESC        PIC S9(3)V COMP-3.
           10  86-ITPO-TX-JURO-DESC        PIC X(30).
           10  86-IRSUMO-TPO-TX-JURO       PIC X(15).
           10  86-CSGL-TPO-TX-JURO         PIC X(6).
           10  86-CIDTFD-TX-JURO           PIC S9(5)V COMP-3.
           10  86-HULT-ATULZ               PIC X(26).
           10  86-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  86-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  86-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  86-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  86-RETORNO.
       05  86-COD-RETORNO                         PIC 9(04).
       05  86-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  86-PROXIMO-RESTART                     PIC 9(05).
       05  86-QTDE-RETORNADA                      PIC 9(03).
       05  86-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  86-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T86.
           10  86RT-CTPO-TX-JURO-DESC      PIC S9(3)V COMP-3.
           10  86RT-ITPO-TX-JURO-DESC      PIC X(30).
           10  86RT-IRSUMO-TPO-TX-JURO     PIC X(15).
           10  86RT-CSGL-TPO-TX-JURO       PIC X(6).
           10  86RT-CIDTFD-TX-JURO         PIC S9(5)V COMP-3.
           10  86RT-HULT-ATULZ             PIC X(26).
           10  86RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  86RT-CTERM                  PIC X(8).
