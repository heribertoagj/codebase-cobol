      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I1 - DB2PRD.TTRASI_RUTIL_INSTR     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I1-ARGUMENTOS-ENTRADA.

       05  I1-COLUNAS-DA-TABELA.
           10  I1-CBCO                         PIC S9(3)V COMP-3.
           10  I1-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  I1-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  I1-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  I1-NSEQ-INSTR-PCELA             PIC S9(2)V COMP-3.
           10  I1-DINSTR-DESC-PCELA            PIC X(10).
           10  I1-VINSTR-DESC-PCELA            PIC S9(15)V9(2) COMP-3.
           10  I1-PINSTR-DESC-PCELA            PIC S9(3)V9(3) COMP-3.
           10  I1-HULT-ATULZ                   PIC X(26).
           10  I1-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  I1-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I1-RETORNO.
       05  I1-COD-RETORNO                         PIC 9(04).
       05  I1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I1-PROXIMO-RESTART                     PIC 9(05).
       05  I1-QTDE-RETORNADA                      PIC 9(03).
       05  I1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI1.
           10  I1RT-CBCO                       PIC S9(3)V COMP-3.
           10  I1RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  I1RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  I1RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  I1RT-NSEQ-INSTR-PCELA           PIC S9(2)V COMP-3.
           10  I1RT-DINSTR-DESC-PCELA          PIC X(10).
           10  I1RT-VINSTR-DESC-PCELA          PIC S9(15)V9(2) COMP-3.
           10  I1RT-PINSTR-DESC-PCELA          PIC S9(3)V9(3) COMP-3.
           10  I1RT-HULT-ATULZ                 PIC X(26).
           10  I1RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  I1RT-CTERM                      PIC X(8).
