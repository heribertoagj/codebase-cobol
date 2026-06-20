      *
      *----------------------------------------------------------------*
      *  MOVIMENTO DE/PARA CESTAS E TARIFAS HSBC/BRADESCO              *
      *  MOVIMENTO DE CORRETORAS                                       *
      *  BOOK GFCTWG00                                                 *
      *  LRECL 0104                                                    *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  G00-CORRET-HSBC-REG.
           05 G00-CORRET-HSBC-HEADER.
              10 G00-COR-H-TP-REG.
                 15 G00-COR-H-TP-REG-R     PIC  9(001).
              10 G00-COR-H-DT-GER.
                 15 G00-COR-H-DT-GER-R     PIC  9(008).
              10 G00-COR-H-NM-REG          PIC  X(030).
              10 FILLER                    PIC  X(065).
           05 G00-CORRET-HSBC-DETALHE      REDEFINES
              G00-CORRET-HSBC-HEADER.
              10 G00-COR-D-TP-REG.
                 15 G00-COR-D-TP-REG-R     PIC  9(001).
              10 G00-COR-D-AGEN.
                 15 G00-COR-D-AGEN-R       PIC  9(005).
              10 G00-COR-D-CTA.
                 15 G00-COR-D-CTA-R        PIC  9(013).
              10 G00-COR-D-CPF-CNPJ.
ST2507*          15 G00-COR-D-CPF-CNPJ-R   PIC  9(009).
ST2507           15 G00-COR-D-CPF-CNPJ-R   PIC  X(009).
              10 G00-COR-D-FILIAL.
ST2507*          15 G00-COR-D-FILIAL-R     PIC  9(004).
ST2507           15 G00-COR-D-FILIAL-R     PIC  X(004).
              10 G00-COR-D-CTR.
                 15 G00-COR-D-CTR-R        PIC  9(002).
              10 G00-COR-D-AM-APUR.
                 15 G00-COR-D-AM-APU-R     PIC  9(006).
              10 G00-COR-D-SD-ACOES.
                 15 G00-COR-D-SD-ACOES-R   PIC  9(015)V9(2).
              10 G00-COR-D-SD-TESO.
                 15 G00-COR-D-SD-TESO-R    PIC  9(015)V9(2).
              10 FILLER                    PIC  X(030).
           05 G00-CORRET-HSBC-TRAILLER     REDEFINES
              G00-CORRET-HSBC-HEADER.
              10 G00-COR-T-TP-REG.
                 15 G00-COR-T-TP-REG-R     PIC  9(001).
              10 G00-COR-T-QTD-REGS.
                 15 G00-COR-T-QTD-REGS-R   PIC  9(015).
              10 FILLER                    PIC  X(088).

      *
      *------------------ FIM DA BOOK GFCTWG00 ------------------------*
      *
