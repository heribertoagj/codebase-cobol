      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E3 - DB2PRD.TSMULA_FORNC_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  E3-ARGUMENTOS-ENTRADA.

       05  E3-COLUNAS-DA-TABELA.
           10  E3-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  E3-NFORNC-DESC-COML             PIC S9(4)V COMP-3.
           10  E3-CDOCTO-FSCAL-DESC            PIC X(20).
           10  E3-CSERIE-DOCTO-FSCAL           PIC X(5).
           10  E3-DEMIS-DOCTO-FSCAL            PIC X(10).
           10  E3-DVCTO-DOCTO-FSCAL            PIC X(10).
           10  E3-VDOCTO-FSCAL                 PIC S9(15)V9(2) COMP-3.
           10  E3-CCONDC-PGTO-FORNC            PIC S9(5)V COMP-3.
           10  E3-CIDTFD-SEQ-PGTO              PIC X(16).
           10  E3-CORIGE-PGTO-FORNC            PIC S9(1)V COMP-3.
           10  E3-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  E3-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  E3-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  E3-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  E3-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  E3-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  E3-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  E3-CNRO-SEQ-TITLO               PIC S9(3)V COMP-3.
           10  E3-CINDCD-SELEC-DESC            PIC X(1).
           10  E3-HULT-ATULZ                   PIC X(26).
           10  E3-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E3-CTERM                        PIC X(8).
           10  E3-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E3-RETORNO.
       05  E3-COD-RETORNO                         PIC 9(04).
       05  E3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E3-PROXIMO-RESTART                     PIC 9(05).
       05  E3-QTDE-RETORNADA                      PIC 9(03).
       05  E3-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE3.
           10  E3RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  E3RT-NFORNC-DESC-COML           PIC S9(4)V COMP-3.
           10  E3RT-CDOCTO-FSCAL-DESC          PIC X(20).
           10  E3RT-CSERIE-DOCTO-FSCAL         PIC X(5).
           10  E3RT-DEMIS-DOCTO-FSCAL          PIC X(10).
           10  E3RT-DVCTO-DOCTO-FSCAL          PIC X(10).
           10  E3RT-VDOCTO-FSCAL               PIC S9(15)V9(2) COMP-3.
           10  E3RT-CCONDC-PGTO-FORNC          PIC S9(5)V COMP-3.
           10  E3RT-CIDTFD-SEQ-PGTO            PIC X(16).
           10  E3RT-CORIGE-PGTO-FORNC          PIC S9(1)V COMP-3.
           10  E3RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  E3RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  E3RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  E3RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  E3RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  E3RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  E3RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  E3RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  E3RT-CINDCD-SELEC-DESC          PIC X(1).
           10  E3RT-HULT-ATULZ                 PIC X(26).
           10  E3RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E3RT-CTERM                      PIC X(8).
           10  E3RT-DINCL-REG                  PIC X(10).
