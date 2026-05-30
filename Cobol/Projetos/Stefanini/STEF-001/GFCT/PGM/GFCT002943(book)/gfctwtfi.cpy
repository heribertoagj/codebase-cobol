      *----------------------------------------------------------------*
      * BOOK GFCTWTFI - ARQUIVO DEMAIS TITULARES BRADESCO EXPRESSO     *
      *                 LRECL = 1089                                   *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWTFI-REG.
           03  GFCTWTFI-AG-CTA        PIC  9(012).
           03  GFCTWTFI-AG-CTA-PAB    PIC  9(015).
           03  GFCTWTFI-AGENCIA       PIC  9(005).
           03  GFCTWTFI-CONTA         PIC  9(007).
           03  GFCTWTFI-RAZAO         PIC  9(005).
           03  GFCTWTFI-DT-ABERT      PIC  X(010).
           03  GFCTWTFI-TP-PESSOA     PIC  X(002).
           03  GFCTWTFI-TP-CONTA      PIC  9(002).
           03  GFCTWTFI-TITULAR       PIC  X(040).
           03  GFCTWTFI-SEXO          PIC  X(001).
           03  GFCTWTFI-DT-NASC       PIC  X(010).
           03  GFCTWTFI-COD-OCUP      PIC  9(006).
           03  GFCTWTFI-TP-DOCTO      PIC  X(020).
ST2507*    03  GFCTWTFI-CGC-CPF       PIC  9(016).
ST2507     03  GFCTWTFI-CGC-CPF       PIC  X(015).
           03  GFCTWTFI-EST-CIVIL     PIC  9(001).
           03  GFCTWTFI-TITULAR2      PIC  X(040).
           03  GFCTWTFI-SEXO2         PIC  X(001).
           03  GFCTWTFI-DT-NASC2      PIC  X(010).
           03  GFCTWTFI-TP-DOCTO2     PIC  X(020).
           03  GFCTWTFI-COD-DOCTO2    PIC  X(015).
           03  GFCTWTFI-CGC-CPF2      PIC  9(016).
           03  GFCTWTFI-ENDERECO      PIC  X(040).
           03  GFCTWTFI-NRO-IMOV      PIC  X(007).
           03  GFCTWTFI-COMPL-ENDER   PIC  X(020).
           03  GFCTWTFI-BAIRRO        PIC  X(020).
           03  GFCTWTFI-MUNICIPIO     PIC  X(025).
           03  GFCTWTFI-UF            PIC  X(002).
           03  GFCTWTFI-CEP           PIC  9(009).
           03  GFCTWTFI-ENDER-COMPAC  PIC  X(040).
           03  GFCTWTFI-NOME-PAI      PIC  X(040).
           03  GFCTWTFI-NOME-MAE      PIC  X(040).
           03  GFCTWTFI-FONE          PIC  9(013).
           03  GFCTWTFI-FAX           PIC  9(013).
           03  GFCTWTFI-FONE-COML     PIC  9(013).
           03  GFCTWTFI-DT-ULT-ATUALIZ PIC X(010).
           03  GFCTWTFI-EMAIL         PIC  X(035).
           03  GFCTWTFI-FONE-PAGER    PIC  9(011).
           03  GFCTWTFI-PAGER         PIC  X(009).
           03  GFCTWTFI-DT-EMIS       PIC  X(010).
           03  GFCTWTFI-DT-EMIS2      PIC  X(010).
           03  GFCTWTFI-NATURALIDADE  PIC  X(030).
           03  GFCTWTFI-NATURALIDADE2 PIC  X(030).
           03  GFCTWTFI-NOME-PAI2     PIC  X(040).
           03  GFCTWTFI-NOME-MAE2     PIC  X(040).
           03  GFCTWTFI-NOME-CONJUGE  PIC  X(040).
           03  GFCTWTFI-NOME-CONJUGE2 PIC  X(040).
           03  GFCTWTFI-POSTO         PIC  9(003).
           03  GFCTWTFI-TP-POSTO      PIC  9(002).
           03  GFCTWTFI-VRENDA        PIC  9(013)V99.
           03  GFCTWTFI-VRENDA-MES-PF2 PIC 9(013)V99.
           03  GFCTWTFI-SITUACAO-CTA  PIC  X(020).
           03  GFCTWTFI-DRCADT        PIC  X(010).
           03  GFCTWTFI-CCAPAC-CVIL   PIC  9(001).
           03  GFCTWTFI-QANO-EMPRG    PIC  9(003).
           03  GFCTWTFI-QMES-EMPRG    PIC  9(003).
           03  GFCTWTFI-ICARGO        PIC  X(040).
           03  GFCTWTFI-TAB-DEMAIS-TIT PIC 9(002).
           03  GFCTWTFI-CSGMTO        PIC  9(003).
           03  GFCTWTFI-CESTA         PIC  9(005).
           03  GFCTWTFI-ICESTA        PIC  X(100).
           03  GFCTWTFI-VTARIFA-CESTA PIC  9(011)V99.
           03  GFCTWTFI-VDEB-CTA-CLI  PIC  9(011)V99.
ST2507     03  FILLER                 PIC  X(001).
