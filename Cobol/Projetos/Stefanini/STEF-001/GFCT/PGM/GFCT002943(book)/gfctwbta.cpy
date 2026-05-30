      *                                                                 00000100
      ******************************************************************00000200
      * NOME DA INC - GFCTWBTA                                         *00000300
      * DESCRICAO   - ARQUIVO DE ENVIO A CIELO - POS                   *00000400
      * TAMANHO     - 145 BYTES                                        *00000500
      * DATA        - ABR/2014                                         *00000600
      * RESPONSAVEL - PAGNOCCA           - SONDA IT                    *00000700
      *                                                                *00000800
      * WBTA-TP-REG:                                                   *00000900
      * 0 - HEADER - COM DATA DE GERACAO (TIMESTAMP)                   *00001000
      * 1 - DETALHE - INFORMACOES CIELO                                *00001100
      * 9 - TRAILLER - TOTAL DE REGISTROS (HEADER E TRAILLER INCLUSOS) *00001200
      *                                                                *00001300
      * OS REGISTROS PRECISAM VIR NA SEQUENCIA: 0, 1, 1, 1, 9          *00001400
      * PARA NENHUM DETALHE: 0, 9 (TOTAL DE REGISTROS = 2)             *00001500
      *                                                                *00001600
      * WBTA-SQ-REG: NUMERO SEQUENCIAL                                 *00001700
      *                                                                *00001800
      * WBTA-CD-RETORNO (UNICO CAMPO A SER ATUALIZADO, DIFERENTE DE    *00001900
      * BRANCOS):                                                      *00002000
      * '  ' - NAO PROCESSADO (ORIGINAL)                               *00002100
      * '00' - PROCESSADO OK                                           *00002200
      * '04' - CLIENTE NAO ENCONTRADO                                  *00002300
      * '08' - ERRO NO PROCESSAMENTO                                   *00002400
      *================================================================*00002500
ST2507*               U L T I M A   A L T E R A C A O                *  00002510
      *----------------------------------------------------------------*00002520
ST2507*    ANALISTA....:  STEFANINI                                    *00002530
ST2507*    DATA........:  07 / 2025                                    *00002540
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *00002550
      *================================================================*00002560
      *                                                                 00002600
       01  GFCTWBTA-REG.                                                00002700
           05  WBTA-SQ-REG                      PIC  9(011).            00002800
           05  WBTA-TP-REG                      PIC  X(001).            00002900
           05  WBTA-REG-HEADER.                                         00003000
               10 WBTA-AAAMM-GERACAO            PIC  9(006).            00003100
               10 WBTA-TMS-GERACAO              PIC  X(026).            00003200
               10 WBTA-TMS-RECEPCAO             PIC  X(026).            00003300
               10 FILLER                        PIC  X(075).            00003400
           05  WBTA-REG-DETALHE REDEFINES WBTA-REG-HEADER.              00003500
              15  WBTA-CJUNC-DEPDC              PIC  9(005).            00003600
              15  WBTA-CCTA-CLI                 PIC  9(007).            00003700
              15  WBTA-CNPJ.                                            00003800
ST2507*           20  WBTA-CNPJ-PRINC           PIC  9(009).            00003900
ST2507*           20  WBTA-CNPJ-FILIAL          PIC  9(004).            00004000
ST2507            20  WBTA-CNPJ-PRINC           PIC  X(009).            00004010
ST2507            20  WBTA-CNPJ-FILIAL          PIC  X(004).            00004020
                  20  WBTA-CNPJ-CONTR           PIC  9(002).            00004100
              15  WBTA-QFRANQ-COMPO-PCOTE       PIC  9(003).            00004200
              15  WBTA-RESERVA.                                         00004300
                  20  WBTA-VCOMPO-TARIF         PIC  9(009)V9(2).       00004400
                  20  WBTA-VEXCED-COMPO-PCOTE   PIC  9(009)V9(2).       00004500
                  20  WBTA-VCESTA-TARIF         PIC  9(009)V9(2).       00004600
                  20  WBTA-CCESTA-TARIF         PIC  9(005).            00004700
                  20  WBTA-CTARIFA-TARIF        PIC  9(005).            00004800
                  20  WBTA-DINIC-ADSAO-INDVD    PIC  X(010).            00004900
                  20  WBTA-DFIM-ADSAO-INDVD     PIC  X(010).            00005000
              15  WBTA-CD-RETORNO               PIC  X(002).            00005100
              15  FILLER                        PIC  X(038).            00005200
           05  WBTA-REG-TRAILLER REDEFINES WBTA-REG-HEADER.             00005300
               10 WBTA-TOT-REGS                 PIC  9(011).            00005400
               10 FILLER                        PIC  X(122).            00005500
      *                                                                 00005600
      ******************************************************************00005700
      *                                                                 00005800
