      *
      *----------------------------------------------------------------*
      *  AREA DE COMUNICACAO DE/PARA DE CONTAS HSBC                    *
      *  BOOK GFCTWF04                                                 *
      *  LRECL 0180                                                    *
      *----------------------------------------------------------------*
      * F04-TAR-COD-RET....- CODIGO DE RETORNO
      *  00 - DE/PARA OK
      *  04 - AG/CTA HSBC NAO LOCALIZADA
      *  08 - AG/CTA HSBC INVALIDA
      *  12 - ERRO SISTEMICO
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
       01  F04-DP-CONTAS-HSBC-REG.
           05 F04-AGE-HSBC                 PIC  9(05).
           05 F04-CTA-HSBC                 PIC  9(13).
           05 F04-T-CTA-HSBC               PIC  9(01).
           05 F04-PESQ-CLIE                PIC  X(01).
           05 F04-AGE-BRAD                 PIC  9(05).
           05 F04-CTA-BRAD                 PIC  9(07).
           05 F04-FJ-BRAD                  PIC  X(01).
ST2507*    05 F04-CGC-BRAD                 PIC  9(09).
ST2507*    05 F04-FIL-BRAD                 PIC  9(05).
ST2507     05 F04-CGC-BRAD                 PIC  X(09).
ST2507     05 F04-FIL-BRAD                 PIC  X(04).
           05 F04-CTR-BRAD                 PIC  9(02).
           05 F04-POSTO-BRAD               PIC  9(05).
           05 F04-SEGM-BRAD                PIC  9(05).
           05 F04-NOME-BRAD                PIC  X(70).
           05 F04-CIDCLI-BRAD              PIC  X(26).
           05 F04-RAZAO-BRAD               PIC  9(05).
           05 F04-COD-RET                  PIC  9(02).
ST2507     05 F04-TAR-RESERVA              PIC  X(19).
      *
      *------------------ FIM DA BOOK GFCTWF04 ------------------------*
      *
