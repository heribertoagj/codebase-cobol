      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0A3 - DB2PRD.TFORNC_DESC_COML       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  A3-ARGUMENTOS-ENTRADA.

       05  A3-COLUNAS-DA-TABELA.
           10  A3-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  A3-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  A3-NFORNC-DESC-COML             PIC S9(4)V COMP-3.
           10  A3-CDOCTO-FSCAL-DESC            PIC X(20).
           10  A3-CSERIE-DOCTO-FSCAL           PIC X(5).
           10  A3-DEMIS-DOCTO-FSCAL            PIC X(10).
           10  A3-DVCTO-DOCTO-FSCAL            PIC X(10).
           10  A3-VDOCTO-FSCAL                 PIC S9(15)V9(2) COMP-3.
           10  A3-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  A3-CCONDC-PGTO-FORNC            PIC S9(5)V COMP-3.
           10  A3-CIDTFD-SEQ-PGTO              PIC X(16).
           10  A3-CORIGE-PGTO-FORNC            PIC S9(1)V COMP-3.
           10  A3-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  A3-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  A3-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  A3-CINDCD-CONF-UTILZ            PIC X(1).
           10  A3-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  A3-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  A3-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  A3-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  A3-CNRO-SEQ-TITLO               PIC S9(3)V COMP-3.
           10  A3-HULT-ATULZ                   PIC X(26).
           10  A3-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  A3-CTERM                        PIC X(8).
           10  A3-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A3-RETORNO.
       05  A3-COD-RETORNO                         PIC 9(04).
       05  A3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A3-PROXIMO-RESTART                     PIC 9(05).
       05  A3-QTDE-RETORNADA                      PIC 9(03).
       05  A3-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TA3.
           10  A3RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  A3RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  A3RT-NFORNC-DESC-COML           PIC S9(4)V COMP-3.
           10  A3RT-CDOCTO-FSCAL-DESC          PIC X(20).
           10  A3RT-CSERIE-DOCTO-FSCAL         PIC X(5).
           10  A3RT-DEMIS-DOCTO-FSCAL          PIC X(10).
           10  A3RT-DVCTO-DOCTO-FSCAL          PIC X(10).
           10  A3RT-VDOCTO-FSCAL               PIC S9(15)V9(2) COMP-3.
           10  A3RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  A3RT-CCONDC-PGTO-FORNC          PIC S9(5)V COMP-3.
           10  A3RT-CIDTFD-SEQ-PGTO            PIC X(16).
           10  A3RT-CORIGE-PGTO-FORNC          PIC S9(1)V COMP-3.
           10  A3RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  A3RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  A3RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  A3RT-CINDCD-CONF-UTILZ          PIC X(1).
           10  A3RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  A3RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  A3RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  A3RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  A3RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  A3RT-HULT-ATULZ                 PIC X(26).
           10  A3RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  A3RT-CTERM                      PIC X(8).
           10  A3RT-DINCL-REG                  PIC X(10).
