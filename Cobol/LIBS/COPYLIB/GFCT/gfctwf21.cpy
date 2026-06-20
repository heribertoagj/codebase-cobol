      *
      *----------------------------------------------------------------*
      *  REGEITADOS DE/PARA CESTAS E TARIFAS HSBC/BRADESCO             *
      *  MOVIMENTO DE CORRETORAS                                       *
      *  BOOK GFCTWF21                                                 *
      *  LRECL 0170                                                    *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  F21-REGISTRO.
           05 F21-CORRET-HSBC-DETALHE.
              10 F21-COR-D-TP-REG.
                 15 F21-COR-D-TP-REG-R     PIC  9(001).
              10 F21-COR-D-AGEN.
                 15 F21-COR-D-AGEN-R       PIC  9(005).
              10 F21-COR-D-CTA.
                 15 F21-COR-D-CTA-R        PIC  9(013).
              10 F21-COR-D-CPF-CNPJ.
ST2507*          15 F21-COR-D-CPF-CNPJ-R   PIC  9(009).
ST2507           15 F21-COR-D-CPF-CNPJ-R   PIC  X(009).
              10 F21-COR-D-FILIAL.
ST2507*          15 F21-COR-D-FILIAL-R     PIC  9(004).
ST2507           15 F21-COR-D-FILIAL-R     PIC  X(004).
              10 F21-COR-D-CTR.
                 15 F21-COR-D-CTR-R        PIC  9(002).
              10 F21-COR-D-AM-APUR.
                 15 F21-COR-D-AM-APU-R     PIC  9(006).
              10 F21-COR-D-SD-ACOES.
                 15 F21-COR-D-SD-ACOES-R   PIC  9(015)V9(2).
              10 F21-COR-D-SD-TESO.
                 15 F21-COR-D-SD-TESO-R    PIC  9(015)V9(2).
              10 FILLER                    PIC  X(030).
           05 F21-MSG-ERRO                 PIC  X(030).
           05 F21-RESERVA                  PIC  X(036).
      *
      *------------------ FIM DA BOOK GFCTWF21 ------------------------*
      *
