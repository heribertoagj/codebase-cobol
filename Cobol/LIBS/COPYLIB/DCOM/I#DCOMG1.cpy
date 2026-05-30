      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G1 - DB2PRD.TTRASI_GARNT_OPER      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G1-ARGUMENTOS-ENTRADA.

       05  G1-COLUNAS-DA-TABELA.
           10  G1-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G1-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G1-CTPO-GARNT                   PIC S9(3)V COMP-3.
           10  G1-HULT-ATULZ                   PIC X(26).
           10  G1-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G1-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G1-RETORNO.
       05  G1-COD-RETORNO                         PIC 9(04).
       05  G1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G1-PROXIMO-RESTART                     PIC 9(05).
       05  G1-QTDE-RETORNADA                      PIC 9(03).
       05  G1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG1.
           10  G1RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G1RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G1RT-CTPO-GARNT                 PIC S9(3)V COMP-3.
           10  G1RT-HULT-ATULZ                 PIC X(26).
           10  G1RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G1RT-CTERM                      PIC X(8).
