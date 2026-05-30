      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0D5 - DB2PRD.TRELAC_CHEQ_IMPRE      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * ANO DA OPERACAO/SEQUENCIA DA OPERACAO/PARCELA/NRO. CHEQUE     *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  W5-ARGUMENTOS-ENTRADA.
           05  W5-DANO-OPER-E                  PIC S9(4)  COMP-3.
           05  W5-NSEQ-OPER-E                  PIC S9(9)  COMP-3.
           05  W5-NPCELA-E                     PIC S9(5)  COMP-3.
           05  W5-CCHEQ-E                      PIC S9(7)  COMP-3.

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  W5-ARGUMENTOS-SAIDA.
           05  W5-COD-RETORNO                  PIC 9(04).
           05  W5-MENSAGEM                     PIC X(79).
           05  W5-DANO-OPER-S                  PIC 9(04).
           05  W5-NSEQ-OPER-S                  PIC 9(09).
           05  W5-NPCELA-S                     PIC 9(05).
           05  W5-CBCO-S                       PIC 9(03).
           05  W5-CAG-BCRIA-S                  PIC 9(05).
           05  W5-CCTA-CORR-BCRIA-S            PIC 9(11).
           05  W5-CCHEQ-S                      PIC 9(07).
           05  W5-CFLIAL-CNPJ-S                PIC 9(05).
           05  W5-CCNPJ-CPF-S                  PIC 9(09).
           05  W5-CCTRL-CNPJ-CPF-S             PIC 9(02).
           05  W5-DVCTO-CHEQ-CTDIA-S           PIC X(10).
           05  W5-VCHEQC-S                     PIC 9(13)V99.
