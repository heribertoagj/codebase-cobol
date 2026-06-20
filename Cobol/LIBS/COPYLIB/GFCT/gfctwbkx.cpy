      ******************************************************************
      * NOME BOOK : GFCTWBKX                                           *
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
       01 GFCTWBKX-REGISTRO.
         03 GFCTWBKX-REGISTRO-GFCTB0P8.
            05 GFCTWBKX-CBCO-CLI                 PIC S9(03) COMP-3.
            05 GFCTWBKX-CAG-BCRIA                PIC S9(05) COMP-3.
            05 GFCTWBKX-CCTA-BCRIA-CLI           PIC S9(13) COMP-3.
            05 GFCTWBKX-CGRP-SERVC               PIC S9(05) COMP-3.
            05 GFCTWBKX-DINIC-VGCIA-SERVC        PIC  X(10).
            05 GFCTWBKX-QFRANQ-TARIF-CADTR       PIC S9(07) COMP-3.
            05 GFCTWBKX-CSIST                    PIC  X(04).
            05 GFCTWBKX-CBCO-SALRL               PIC S9(03) COMP-3.
            05 GFCTWBKX-CAG-BCRIA-SALRL          PIC S9(05) COMP-3.
            05 GFCTWBKX-CCTA-BCRIA-SALRL         PIC S9(13) COMP-3.
            05 GFCTWBKX-NFLEXZ-TARIF-AVULS       PIC S9(10) COMP-3.
            05 GFCTWBKX-CPSSOA-JURID-CONTR       PIC S9(10) COMP-3.
            05 GFCTWBKX-CTPO-CONTR-NEGOC         PIC S9(03) COMP-3.
            05 GFCTWBKX-NSEQ-CONTR-NEGOC         PIC S9(10) COMP-3.
            05 GFCTWBKX-DINIC-VGCIA-FLEXZ        PIC  X(10).
            05 GFCTWBKX-DINIC-VGCIA-NUL          PIC  X(01).
            05 GFCTWBKX-DFIM-VGCIA-FLEXZ         PIC  X(10).
            05 GFCTWBKX-DFIM-VGCIA-NUL           PIC  X(01).
         03 GFCTWBKX-REGISTRO-COMPLENTO.
            05 GFCTWBKX-TRF-GFCT                 PIC  9(05).
            05 GFCTWBKX-TRF-LEGADO               PIC  9(05).
            05 GFCTWBKX-VLR-TARIFA               PIC  9(15)V99  COMP-3.
            05 GFCTWBKX-CENTRO-CUSTO             PIC  X(04).
            05 GFCTWBKX-DTA-INI-VIG              PIC  9(08).
            05 GFCTWBKX-DTA-FIM-VIG              PIC  9(08).
            05 GFCTWBKX-CGRUPO                   PIC  9(03) COMP-3.
            05 GFCTWBKX-CSUBGR                   PIC  9(03) COMP-3.
ST2507*     05 GFCTWBKX-CPF-NUM                  PIC  9(09) COMP-3.
ST2507*     05 GFCTWBKX-CPF-FIL                  PIC  9(04).
ST2507      05 GFCTWBKX-CPF-NUM                  PIC  X(09).
ST2507      05 GFCTWBKX-CPF-FIL                  PIC  X(04).
            05 GFCTWBKX-CPF-CON                  PIC  9(02).
ST2507*     05 FILLER                            PIC  X(09).
ST2507      05 FILLER                            PIC  X(05).
