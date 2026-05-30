      *================================================================*
      *                    TECH MAHINDRA                               *
      *----------------------------------------------------------------*
      *    BOOK........:   DCOMW676                                    *
      *    ANALISTA....:   CEZAR VARGAS FERNANDES   - TECH MAHINDRA    *
      *    ANALISTA DDS:   RICARDO JAMMAL           - BRADESCO / GP.39 *
      *    DATA........:   20/06/2022                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   BOOK DO ARQUIVO INFORMACOES DAS BAIXAS      *
      *                    CONTABILIZADAS EM PREJUIZO SEM ESTORNO      *
      *                                                                *
      *    ORGANIZACAO.:   SEQUENCIAL                                  *
      *    TAMANHO.....:   400                                         *
      *----------------------------------------------------------------*

       01  FD-BXPREJU-REG.
  1 10     05  FD-DATA-BASE            PIC  X(010).
 11  5     05  FD-EMPRESA              PIC  9(005).
 16  4     05  FD-SISTEMA              PIC  X(004).
 20  5     05  FD-AGENCIA              PIC  9(005).
 25 13     05  FD-CONTA                PIC  9(013).
 39  3     05  FD-CARTEIRA             PIC  9(003).
 41 17     05  FD-CONTRATO             PIC  9(017).
 58 16     05  FD-CPF-CNPJ-NRO         PIC  9(009).
 58 16     05  FD-TIPO-PSSOA           PIC  X(001).
 58 16     05  FD-CPF-CNPJ-FLIAL       PIC  9(004).
 58 16     05  FD-CPF-CNPJ-CTRL        PIC  9(002).
 74 41     05  FD-NOME                 PIC  X(040).
114  1     05  FD-SINAL-VDESCONTO      PIC  X(001).
115 17     05  FD-VLR-DESCONTO         PIC  9(015)V99.
132  1     05  FD-SINAL-VDIVIDA        PIC  X(001).
133 18     05  FD-VLR-DIVIDA           PIC  9(015)V99.
150  1     05  FD-SINAL-VPAGO          PIC  X(001).
151 18     05  FD-VLR-PAGO             PIC  9(015)V99.
168 11     05  FD-DATA-MOVIMENTO       PIC  X(010).
179 36     05  FILLER                  PIC  X(037).
215  5     05  FD-AGENCIA-CONTABIL     PIC  9(005).
220  5     05  FD-RAZAO                PIC  X(005).
225  7     05  FD-CONTA-CONTABIL-NR    PIC  9(007).
232  1     05  FD-CONTA-CONTABIL-DG    PIC  X(001).
233  4     05  FD-DANO-OPER-DESC       PIC  9(004).
237  9     05  FD-NSEQ-OPER-DESC       PIC  9(009).
246  9     05  FD-NPCELA-DESC-COML     PIC  9(009).
255  1     05  FD-CTPO-BAIXA-PCELA     PIC  X(001).
256  3     05  FD-CPRODT               PIC  9(003).
259  3     05  FD-CSPROD-DESC-COML     PIC  9(003).
262  3     05  FD-CMEIO-ENTRD-DESC     PIC  9(003).
265 10     05  FD-DINIC-OPER-DESC      PIC  X(010).
275126     05  FILLER                  PIC  X(126).

      *================================================================*
      *    FIM DO BOOK DCOMW676                                        *
      *================================================================*
