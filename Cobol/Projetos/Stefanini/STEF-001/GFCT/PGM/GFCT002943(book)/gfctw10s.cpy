      ******************************************************************
      * NOME BOOK : GFCTW10S                                           *
      * DESCRICAO : AREA DE SAIDA DE DADOS                             *
      *             CONSULTA DE DADOS NA TABELA:                       *
      *             TABELA CONSOLIDADA DE CLIENTES SEM CESTA SERVICOS  *
      * DATA      : JUNHO/2013                                         *
      * AUTOR     : SONDA PROCWORK                                     *
      * EMPRESA   : ANDRE FERNANDES VIEIRA                             *
      * GRUPO     : GFCT                                               *
      * TAMANHO   : 10316                                              *
      ******************************************************************
      * GFCTW10S-S-IPSSOA-COPLT          = NOME DO CLIENTE (COMPLETO)  *
      * GFCTW10S-S-CSERVC-TARIF          = TIPO DE CESTA               *
      * GFCTW10S-S-CINDCD-VINCLO-FL      = VINCULO FOLHA DE PAGAMENTO  *
      *                                    (1)=SIM OU (2)=NAO          *
      * GFCTW10S-S-CCTA-BCRIA            = CONTA DO CLIENTE            *
      * GFCTW10S-S-DABERT-CTA            = DATA ABERTURA CONTA         *
      *                                    (DD.MM.AAAA)                *
      * GFCTW10S-S-SINAL-VRENTB          = SINAL (-) OU (+)            *
      * GFCTW10S-S-VINVES                = VALOR DO INVESTIMENTO       *
      * GFCTW10S-S-SINAL-VINVES          = SINAL (-) OU (+)            *
      * GFCTW10S-S-CDDI                  = CODIGO DDI                  *
      * GFCTW10S-S-CDDD                  = CODIGO DDD                  *
      * GFCTW10S-S-CID-RMAL-BIP-PAGER    = CODIGO IDENTIFICACAO RAMAL  *
      * GFCTW10S-S-NLIN-TFONI            = NUMERO DO TELEFONE          *
      * GFCTW10S-S-CINDCD-REST-CNTAT     = INDICADOR RESTRICAO         *
      *                                    (1)=SIM OU (2)=NAO          *
      * GFCTW10S-S-CCPF-CNPJ             = NUMERO DO CPF               *
      * GFCTW10S-S-CCTRL-CPF-CNPJ        = NUMERO CONTROLE CPF         *
      * GFCTW10S-S-CPSSOA                = NUMERO DO CLUBE             *
      * GFCTW10S-S-CFUNC-BDSCO           = CODIGO FUNCIONAL DO GERENTE *
      * GFCTW10S-S-CAG-BCRIA             = CODIGO DA AGENCIA CLIENTE   *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
      *================================================================*

        05 GFCTW10S-HEADER.
           10 GFCTW10S-COD-LAYOUT          PIC X(08) VALUE 'GFCTW10S'.
           10 GFCTW10S-TAM-LAYOUT          PIC 9(05) VALUE  10316.
        05 GFCTW10S-REGISTRO-SAIDA.
           10 GFCTW10S-QTD-LISTA-OCORR            PIC 9(03).
           10 GFCTW10S-REG-LISTA-OCORR            OCCURS 50 TIMES
              DEPENDING ON GFCTW10S-QTD-LISTA-OCORR.
              20 GFCTW10S-S-IPSSOA-COPLT          PIC X(70).
              20 GFCTW10S-S-CSERVC-TARIF          PIC 9(05).
              20 GFCTW10S-S-CINDCD-VINCLO-FL      PIC 9(01).
              20 GFCTW10S-S-CCTA-BCRIA            PIC 9(13).
              20 GFCTW10S-S-DABERT-CTA            PIC X(10).
              20 GFCTW10S-S-VRENTB                PIC 9(15)V9(02).
              20 GFCTW10S-S-SINAL-VRENTB          PIC X(01).
              20 GFCTW10S-S-VINVES                PIC 9(16)V9(02).
              20 GFCTW10S-S-SINAL-VINVES          PIC X(01).
              20 GFCTW10S-S-CDDI                  PIC X(04).
              20 GFCTW10S-S-CID-RMAL-BIP-PAGER    PIC X(15).
              20 GFCTW10S-S-NLIN-TFONI            PIC 9(11).
              20 GFCTW10S-S-CINDCD-REST-CNTAT     PIC 9(01).
ST2507*       20 GFCTW10S-S-CCPF-CNPJ             PIC 9(09).
ST2507        20 GFCTW10S-S-CCPF-CNPJ             PIC X(09).
              20 GFCTW10S-S-CCTRL-CPF-CNPJ        PIC 9(02).
              20 GFCTW10S-S-CPSSOA                PIC 9(10).
              20 GFCTW10S-S-CFUNC-BDSCO           PIC 9(09).
              20 GFCTW10S-S-CAG-BCRIA             PIC 9(05).
      ******************************************************************
      *                        FINAL DO BOOK                           *
      ******************************************************************
