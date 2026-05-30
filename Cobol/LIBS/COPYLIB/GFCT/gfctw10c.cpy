      ******************************************************************
      * NOME BOOK  : GFCTW10C - INTERFACE SERVICO BASICO               *
      * FUNCAO     : CONSULTA LISTA DE CLIENTES SEM CESTA              *
      *              TABELA: (TCLI_SEM_CESTA)                          *
      *              TABELA CONSOLIDADA DE CLIENTES SEM CESTA          *
      * DATA       : JUNHO/2013                                        *
      * AUTOR      : ANDRE FERNANDES VIEIRA                            *
      * EMPRESA    : SONDAPROCWORK                                     *
      * GRUPO      : GFCT                                              *
      * TAMANHO    : 10099                                             *
      ******************************************************************
      * GFCTW10C-S-IPSSOA-COPLT          = NOME DO CLIENTE (COMPLETO)  *
      * GFCTW10C-S-CSERVC-TARIF          = TIPO DE CESTA               *
      * GFCTW10C-S-CINDCD-VINCLO-FL      = VINCULO FOLHA DE PAGAMENTO  *
      *                                    (1)=SIM OU (2)=NAO          *
      * GFCTW10C-S-CCTA-BCRIA            = CONTA DO CLIENTE            *
      * GFCTW10C-S-DABERT-CTA            = DATA ABERTURA CONTA         *
      *                                    (DD.MM.AAAA)                *
      * GFCTW10C-S-VRENTB                = VALOR RENTABILIDADE         *
      * GFCTW10C-S-SINAL-VRENTB          = SINAL (-) OU (+)            *
      * GFCTW10C-S-VINVES                = VALOR DO INVESTIMENTO       *
      * GFCTW10C-S-SINAL-VINVES          = SINAL (-) OU (+)            *
      * GFCTW10C-S-CDDI                  = CODIGO DDI                  *
      * GFCTW10C-S-CDDD                  = CODIGO DDD                  *
      * GFCTW10C-S-CID-RMAL-BIP-PAGER    = CODIGO IDENTIFICACAO RAMAL  *
      * GFCTW10C-S-NLIN-TFONI            = NUMERO DO TELEFONE          *
      * GFCTW10C-S-CINDCD-REST-CNTAT     = INDICADOR RESTRICAO         *
      *                                    (1)=SIM OU (2)=NAO          *
      * GFCTW10C-S-CCPF-CNPJ             = NUMERO DO CPF               *
      * GFCTW10C-S-CCTRL-CPF-CNPJ        = NUMERO CONTROLE CPF         *
      * GFCTW10C-S-CPSSOA                = NUMERO DO CLUBE             *
      * GFCTW10C-S-CFUNC-BDSCO           = CODIGO FUNCIONAL DO GERENTE *
      ******************************************************************
       05 GFCTW10C-HEADER.
          10 GFCTW10C-COD-LAYOUT          PIC X(08) VALUE 'GFCTW10C'.
          10 GFCTW10C-TAM-LAYOUT          PIC 9(05) VALUE  10099.
      *
       05 GFCTW10C-REGISTRO.
          10 GFCTW10C-E-CFUNC-BDSCO             PIC  9(09).
          10 GFCTW10C-E-CAG-BCRIA               PIC  9(05).
          10 GFCTW10C-BLOCO-PAGINACAO.
            15 GFCTW10C-INDICADOR-PAGINACAO       PIC  X(01).
              88 GFCTW10C-P-INICIAL               VALUE 'I'.
              88 GFCTW10C-P-PRIMEIRA              VALUE 'P'.
              88 GFCTW10C-P-SEGUINTE              VALUE 'S'.
              88 GFCTW10C-P-ANTERIOR              VALUE 'A'.
              88 GFCTW10C-P-ULTIMA                VALUE 'U'.
            15 GFCTW10C-I-CHAVE.
              20 GFCTW10C-I-NUMBER-ROW           PIC 9(009).
            15 GFCTW10C-F-CHAVE.
              20 GFCTW10C-F-NUMBER-ROW           PIC 9(009).
      *
       05 GFCTW10C-REGISTRO-SAIDA.
          10 GFCTW10C-S-QTD-LISTA-OCORR         PIC  9(003).
          10 GFCTW10C-S-REG-LISTA-OCORR OCCURS  0 TO 50 TIMES
             DEPENDING ON GFCTW10C-S-QTD-LISTA-OCORR.
             15 GFCTW10C-S-IPSSOA-COPLT         PIC X(70).
             15 GFCTW10C-S-CSERVC-TARIF         PIC 9(05).
             15 GFCTW10C-S-CINDCD-VINCLO-FL     PIC 9(01).
             15 GFCTW10C-S-CCTA-BCRIA           PIC 9(13).
             15 GFCTW10C-S-DABERT-CTA           PIC X(10).
             15 GFCTW10C-S-VRENTB               PIC 9(15)V9(02).
             15 GFCTW10C-S-SINAL-VRENTB         PIC X(01).
             15 GFCTW10C-S-VINVES               PIC 9(16)V9(02).
             15 GFCTW10C-S-SINAL-VINVES         PIC X(01).
             15 GFCTW10C-S-CDDI                 PIC X(04).
             15 GFCTW10C-S-CDDD                 PIC X(04).
             15 GFCTW10C-S-CID-RMAL-BIP-PAGER   PIC X(15).
             15 GFCTW10C-S-NLIN-TFONI           PIC 9(11).
             15 GFCTW10C-S-CINDCD-REST-CNTAT    PIC 9(01).
             15 GFCTW10C-S-CCPF-CNPJ            PIC 9(09).
             15 GFCTW10C-S-CCTRL-CPF-CNPJ       PIC 9(02).
             15 GFCTW10C-S-CPSSOA               PIC 9(10).
             15 GFCTW10C-S-CFUNC-BDSCO          PIC 9(09).
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
