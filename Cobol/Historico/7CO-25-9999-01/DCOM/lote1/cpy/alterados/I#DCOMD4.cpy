      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0D4 - DB2PRD.TPSSOA_DESC_COML       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  D4-ARGUMENTOS-ENTRADA.

       05  D4-COLUNAS-DA-TABELA.
           10  D4-NPSSOA-DESC-COML             PIC S9(9)V COMP-3.
FS2511     10  D4-CCNPJ-CPF                    PIC X(09).
FS2511     10  D4-CFLIAL-CNPJ                  PIC X(04).
           10  D4-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  D4-CDOCTO-ID                    PIC X(15).
           10  D4-IPSSOA-DESC-COML             PIC X(60).
           10  D4-CCLUB                        PIC S9(10)V COMP-3.
           10  D4-DINCL-REG                    PIC X(10).
           10  D4-HULT-ATULZ                   PIC X(26).
           10  D4-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  D4-CTERM                        PIC X(8).
           10  D4-CINDCD-TPO-PSSOA             PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D4-RETORNO.
       05  D4-COD-RETORNO                         PIC 9(04).
       05  D4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D4-PROXIMO-RESTART                     PIC 9(05).
       05  D4-QTDE-RETORNADA                      PIC 9(03).
       05  D4-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TD4.
           10  D4RT-NPSSOA-DESC-COML           PIC S9(9)V COMP-3.
FS2511     10  D4RT-CCNPJ-CPF                  PIC X(09).
FS2511     10  D4RT-CFLIAL-CNPJ                PIC X(04).
           10  D4RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  D4RT-CDOCTO-ID                  PIC X(15).
           10  D4RT-IPSSOA-DESC-COML           PIC X(60).
           10  D4RT-CCLUB                      PIC S9(10)V COMP-3.
           10  D4RT-DINCL-REG                  PIC X(10).
           10  D4RT-HULT-ATULZ                 PIC X(26).
           10  D4RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  D4RT-CTERM                      PIC X(8).
           10  D4RT-CINDCD-TPO-PSSOA           PIC X(1).
