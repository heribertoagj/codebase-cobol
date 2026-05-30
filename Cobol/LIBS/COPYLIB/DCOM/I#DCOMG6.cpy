      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G6 - DB2PRD.TTRASI_RUTIL_TITLO     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G6-ARGUMENTOS-ENTRADA.

       05  G6-COLUNAS-DA-TABELA.
           10  G6-CBCO                         PIC S9(3)V COMP-3.
           10  G6-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  G6-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  G6-NTITLO-DESC-COML             PIC S9(5)V COMP-3.
           10  G6-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  G6-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  G6-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  G6-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  G6-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  G6-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  G6-CTITLO-COBR-BCO              PIC S9(18)V COMP-3.
           10  G6-CNRO-SEQ-TITLO               PIC S9(3)V COMP-3.
           10  G6-NNOTA-PROMS-RURAL            PIC S9(11)V COMP-3.
           10  G6-CINDCD-NOTA-PROMS            PIC X(3).
           10  G6-CTPO-PROTR-RURAL             PIC S9(2)V COMP-3.
           10  G6-CPRODT-RURAL                 PIC S9(3)V COMP-3.
           10  G6-QPRODT-RURAL                 PIC S9(9)V COMP-3.
           10  G6-CUND-MEDD                    PIC S9(3)V COMP-3.
           10  G6-CINDCD-PGDOR-JURO            PIC X(1).
           10  G6-DEMIS-TITLO                  PIC X(10).
           10  G6-DVCTO-TITLO-DESC             PIC X(10).
           10  G6-VTITLO-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  G6-CSACDO-TITLO-OPER            PIC S9(9)V COMP-3.
           10  G6-CSEU-NRO-TITLO               PIC X(15).
           10  G6-CESPCE-TITLO-COBR            PIC S9(3)V COMP-3.
           10  G6-CINDCD-INSTR-DESC            PIC S9(1)V COMP-3.
           10  G6-CCEP                         PIC S9(5)V COMP-3.
           10  G6-CCEP-COMPL                   PIC S9(3)V COMP-3.
           10  G6-ELOGDR                       PIC X(40).
           10  G6-IBAIRO                       PIC X(40).
           10  G6-IMUN                         PIC X(30).
           10  G6-CSGL-UF                      PIC X(2).
           10  G6-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  G6-HSIT-DESC-COML               PIC X(26).
           10  G6-HULT-ATULZ                   PIC X(26).
           10  G6-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G6-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G6-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G6-RETORNO.
       05  G6-COD-RETORNO                         PIC 9(04).
       05  G6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G6-PROXIMO-RESTART                     PIC 9(05).
       05  G6-QTDE-RETORNADA                      PIC 9(03).
       05  G6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG6.
           10  G6RT-CBCO                       PIC S9(3)V COMP-3.
           10  G6RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  G6RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  G6RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  G6RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  G6RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  G6RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  G6RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  G6RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  G6RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  G6RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  G6RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  G6RT-NNOTA-PROMS-RURAL          PIC S9(11)V COMP-3.
           10  G6RT-CINDCD-NOTA-PROMS          PIC X(3).
           10  G6RT-CTPO-PROTR-RURAL           PIC S9(2)V COMP-3.
           10  G6RT-CPRODT-RURAL               PIC S9(3)V COMP-3.
           10  G6RT-QPRODT-RURAL               PIC S9(9)V COMP-3.
           10  G6RT-CUND-MEDD                  PIC S9(3)V COMP-3.
           10  G6RT-CINDCD-PGDOR-JURO          PIC X(1).
           10  G6RT-DEMIS-TITLO                PIC X(10).
           10  G6RT-DVCTO-TITLO-DESC           PIC X(10).
           10  G6RT-VTITLO-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  G6RT-CSACDO-TITLO-OPER          PIC S9(9)V COMP-3.
           10  G6RT-CSEU-NRO-TITLO             PIC X(15).
           10  G6RT-CESPCE-TITLO-COBR          PIC S9(3)V COMP-3.
           10  G6RT-CINDCD-INSTR-DESC          PIC S9(1)V COMP-3.
           10  G6RT-CCEP                       PIC S9(5)V COMP-3.
           10  G6RT-CCEP-COMPL                 PIC S9(3)V COMP-3.
           10  G6RT-ELOGDR                     PIC X(40).
           10  G6RT-IBAIRO                     PIC X(40).
           10  G6RT-IMUN                       PIC X(30).
           10  G6RT-CSGL-UF                    PIC X(2).
           10  G6RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  G6RT-HSIT-DESC-COML             PIC X(26).
           10  G6RT-HULT-ATULZ                 PIC X(26).
           10  G6RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G6RT-CTERM                      PIC X(8).
