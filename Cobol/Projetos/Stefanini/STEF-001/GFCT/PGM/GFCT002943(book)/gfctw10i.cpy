      ******************************************************************
      * NOME BOOK : GFCTW10I                                           *
      * DESCRICAO : AREA DE INTERFACE - COORDENADOR X BASE             *
      *             CONSULTA DE DADOS NA TABELA:                       *
      *             TABELA CONSOLIDADA CLIENTE SEM CESTA DE SERVICO    *
      * DATA      : JUNHO/2013                                         *
      * AUTOR     : SONDA PROCWORK                                     *
      * EMPRESA   : ANDRE FERNANDES VIEIRA                             *
      * GRUPO     : GFCT                                               *
      * TAMANHO   : 10127                                              *
      ******************************************************************
      * GFCTW10I-S-IPSSOA-COPLT          = NOME DO CLIENTE (COMPLETO)  *
      * GFCTW10I-S-CSERVC-TARIF          = TIPO DE CESTA               *
      * GFCTW10I-S-CINDCD-VINCLO-FL      = VINCULO FOLHA DE PAGAMENTO  *
      *                                    (1)=SIM OU (2)=NAO          *
      * GFCTW10I-S-CCTA-BCRIA            = CONTA DO CLIENTE            *
      * GFCTW10I-S-DABERT-CTA            = DATA ABERTURA CONTA         *
      *                                    (DD.MM.AAAA)                *
      * GFCTW10I-S-SINAL-VRENTB          = SINAL (-) OU (+)            *
      * GFCTW10I-S-VINVES                = VALOR DO INVESTIMENTO       *
      * GFCTW10I-S-SINAL-VINVES          = SINAL (-) OU (+)            *
      * GFCTW10I-S-CDDI                  = CODIGO DDI                  *
      * GFCTW10I-S-CDDD                  = CODIGO DDD                  *
      * GFCTW10I-S-CID-RMAL-BIP-PAGER    = CODIGO IDENTIFICACAO RAMAL  *
      * GFCTW10I-S-NLIN-TFONI            = NUMERO DO TELEFONE          *
      * GFCTW10I-S-CINDCD-REST-CNTAT     = INDICADOR RESTRICAO         *
      *                                    (1)=SIM OU (2)=NAO          *
      * GFCTW10I-S-CCPF-CNPJ             = NUMERO DO CPF               *
      * GFCTW10I-S-CCTRL-CPF-CNPJ        = NUMERO CONTROLE CPF         *
      * GFCTW10I-S-CPSSOA                = NUMERO DO CLUBE             *
      * GFCTW10I-S-CFUNC-BDSCO           = CODIGO FUNCIONAL DO GERENTE *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
       05 GFCTW10I-HEADER.
          10 GFCTW10I-COD-LAYOUT          PIC X(08) VALUE 'GFCTW10I'.
          10 GFCTW10I-TAM-LAYOUT          PIC 9(05) VALUE  10127.

       05 GFCTW10I-REGISTRO.

          10  GFCTW10I-ENTRADA.
            15 GFCTW10I-E-CFUNC-BDSCO             PIC  9(09).
            15 GFCTW10I-E-CAG-BCRIA               PIC  9(05).

          10 GFCTW10I-BLOCO-PAGINACAO.
             15 GFCTW10I-INDICADOR-PAGINACAO      PIC X(01).
               88 GFCTW10I-P-INICIAL              VALUE 'I'.
               88 GFCTW10I-P-PRIMEIRA             VALUE 'P'.
               88 GFCTW10I-P-SEGUINTE             VALUE 'S'.
               88 GFCTW10I-P-ANTERIOR             VALUE 'A'.
               88 GFCTW10I-P-ULTIMA               VALUE 'U'.
             15 GFCTW10I-CHAVE-INI.
              20 GFCTW10I-I-CFUNC-BDSCO           PIC  9(09).
              20 GFCTW10I-I-NUMBER-ROW            PIC  9(09).
            15 GFCTW10I-CHAVE-FIM.
              20 GFCTW10I-F-CFUNC-BDSCO           PIC  9(09).
              20 GFCTW10I-F-CAG-BCRIA             PIC  9(05).
              20 GFCTW10I-F-NUMBER-ROW            PIC  9(09).

          10 GFCTW10I-SAIDA.
            15 GFCTW10I-S-QTD-LISTA-OCORR         PIC 9(03).
            15 GFCTW10I-S-REG-LISTA-OCORR         OCCURS 50 TIMES
                             DEPENDING ON  GFCTW10I-S-QTD-LISTA-OCORR.
              20 GFCTW10I-S-IPSSOA-COPLT          PIC X(70).
              20 GFCTW10I-S-CSERVC-TARIF          PIC 9(05).
              20 GFCTW10I-S-CINDCD-VINCLO-FL      PIC 9(01).
              20 GFCTW10I-S-CCTA-BCRIA            PIC 9(13).
              20 GFCTW10I-S-DABERT-CTA            PIC X(10).
              20 GFCTW10I-S-VRENTB                PIC 9(15)V9(02).
              20 GFCTW10I-S-SINAL-VRENTB          PIC X(01).
              20 GFCTW10I-S-VINVES                PIC 9(16)V9(02).
              20 GFCTW10I-S-SINAL-VINVES          PIC X(01).
              20 GFCTW10I-S-CDDD                  PIC X(04).
              20 GFCTW10I-S-CID-RMAL-BIP-PAGER    PIC X(15).
              20 GFCTW10I-S-NLIN-TFONI            PIC 9(11).
              20 GFCTW10I-S-CINDCD-REST-CNTAT     PIC 9(01).
ST2507*       20 GFCTW10I-S-CCPF-CNPJ             PIC 9(09).
ST2507        20 GFCTW10I-S-CCPF-CNPJ             PIC X(09).
              20 GFCTW10I-S-CCTRL-CPF-CNPJ        PIC 9(02).
              20 GFCTW10I-S-CPSSOA                PIC 9(10).
              20 GFCTW10I-S-CFUNC-BDSCO           PIC 9(09).
      ******************************************************************
      *                        FINAL DO BOOK                           *
      ******************************************************************
