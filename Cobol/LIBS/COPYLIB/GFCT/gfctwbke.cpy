      ******************************************************************
      * NOME BOOK : GFCTWBKE                                           *
      * DESCRICAO : BOOK FORMATADO PARA ENVIO AO CEPT                  *
      * DATA      : 20-03-2024                                         *
      * AUTOR     : MAURICIO ROQUE DA SILVA                            *
      * EMPRESA   : SIS                                                *
      * GRUPO     : GFCT                                               *
      * TAMANHO   : 150                                                *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 GFCTWBKE-REGISTRO.
         03 GFCTWBKE-REGISTRO-GFCTB0P8.
            05 GFCTWBKE-CBCO-CLI                 PIC S9(03) COMP-3.
            05 GFCTWBKE-CAG-BCRIA                PIC S9(05) COMP-3.
            05 GFCTWBKE-CCTA-BCRIA-CLI           PIC S9(13) COMP-3.
            05 GFCTWBKE-CGRP-SERVC               PIC S9(05) COMP-3.
            05 GFCTWBKE-DINIC-VGCIA-SERVC        PIC  X(10).
            05 GFCTWBKE-QFRANQ-TARIF-CADTR       PIC S9(07) COMP-3.
            05 GFCTWBKE-CSIST                    PIC  X(04).
            05 GFCTWBKE-CBCO-SALRL               PIC S9(03) COMP-3.
            05 GFCTWBKE-CAG-BCRIA-SALRL          PIC S9(05) COMP-3.
            05 GFCTWBKE-CCTA-BCRIA-SALRL         PIC S9(13) COMP-3.
            05 GFCTWBKE-NFLEXZ-TARIF-AVULS       PIC S9(10) COMP-3.
            05 GFCTWBKE-CPSSOA-JURID-CONTR       PIC S9(10) COMP-3.
            05 GFCTWBKE-CTPO-CONTR-NEGOC         PIC S9(03) COMP-3.
            05 GFCTWBKE-NSEQ-CONTR-NEGOC         PIC S9(10) COMP-3.
            05 GFCTWBKE-DINIC-VGCIA-FLEXZ        PIC  X(10).
            05 GFCTWBKE-DINIC-VGCIA-NUL          PIC  X(01).
            05 GFCTWBKE-DFIM-VGCIA-FLEXZ         PIC  X(10).
            05 GFCTWBKE-DFIM-VGCIA-NUL           PIC  X(01).
         03 GFCTWBKE-REGISTRO-COMPLENTO.
            05 GFCTWBKE-TRF-GFCT                 PIC  9(05).
            05 GFCTWBKE-TRF-LEGADO               PIC  9(05).
            05 GFCTWBKE-VLR-TARIFA               PIC  9(15)V99  COMP-3.
            05 GFCTWBKE-CENTRO-CUSTO             PIC  X(04).
            05 GFCTWBKE-DTA-INI-VIG              PIC  9(08).
            05 GFCTWBKE-DTA-FIM-VIG              PIC  9(08).
            05 GFCTWBKE-CGRUPO                   PIC  9(03) COMP-3.
            05 GFCTWBKE-CSUBGR                   PIC  9(03) COMP-3.
ST2507*     05 GFCTWBKE-CPF-NUM                  PIC  9(09) COMP-3.
ST2507*     05 GFCTWBKE-CPF-FIL                  PIC  9(04).
ST2507      05 GFCTWBKE-CPF-NUM                  PIC  X(09).
ST2507      05 GFCTWBKE-CPF-FIL                  PIC  X(04).
            05 GFCTWBKE-CPF-CON                  PIC  9(02).
ST2507*     05 FILLER                            PIC  X(09).
ST2507      05 FILLER                            PIC  X(05).
