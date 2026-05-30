      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM3Z5O
      *================================================================*
      *                      F O U R S Y S                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....: DCOM3Z5O                                      *
      *    ANALISTA....: RODRIGO PEREIRA                               *
      *    DATA........: 04/04/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: FORMATAR LAYOUT DICM PARA IMPRESSAO DE        *
      *                  FORMULARIOS NO DOCG                           *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    DCOMW000 BOOK BLOCO DE CONTROLE CHAMADA SERVICOS FUNCIONAIS *
      *    DCOMWZ9I AREA DE COMUNICACAO                                *
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *----------------------------------------------------------------*
      *    MODULOS  - DESCRICAO                                        *
      *    CMFE3JCC - OBTER INFORMACOES DE AVALISTA E GARANTIAS        *
      *    POOL1911 - CALCULO DE DIGITO                                *
      *    FEMP4L1S - OBTER NUMERO DE PROPOSTA DO CONTRATO             *
      *    FEMP4L9S - OBTER OPERACOES DE REFINANCIAMENTO               *
      *    FEMP3GDC - VERIFICAR PESSOA POLITICAMENTE EXPOSTA           *
      *    SBIIC31  - ACESSO A MODULOS IMS                             *
      *    CMFE239B - MODULO IMS PARA OBTER VALOR ESCRITO POR EXTENSO  *
      *----------------------------------------------------------------*
      *                       A L T E R A C O E S                      *
      *----------------------------------------------------------------*
V.001 * 03/05/2023 | DANILO STELLA | AJUSTES HEXAVISION.               *
      *----------------------------------------------------------------*
V.002 * __/__/____ |               |                                   *
      *----------------------------------------------------------------*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  FILLER                      PIC X(032)  VALUE
           '* INICIO DA WORKING DCOM3Z5O *'.
      *
V.001  01  WRK-DATA-OPERACAO           PIC 9(008)  VALUE ZEROS.
.      01  FILLER    REDEFINES    WRK-DATA-OPERACAO.
.          05  WRK-DATA-OPERACAO-RED   PIC X(008).
.     *
.      01  WRK-DATA-VENC-PRRG          PIC 9(008)  VALUE ZEROS.
.      01  FILLER    REDEFINES    WRK-DATA-VENC-PRRG.
V.001      05  WRK-DATA-VENC-PRRG-RED  PIC X(008).

       01  WRK-9DE1.
           05  WRK-XDE1                PIC X(001)  VALUE SPACES.
           05  FILLER                  PIC X(001)  VALUE SPACES.

4S2511*01  WRK-CPF-CNPJ-AUX            PIC 9(015)  VALUE ZEROS.
4S2511 01  WRK-CPF-CNPJ-AUX            PIC X(015)  VALUE SPACES.
       01  FILLER                      REDEFINES WRK-CPF-CNPJ-AUX.
4S2511*           03  WRK-CNPJ-AUX-NUM        PIC 9(009).
4S2511            03  WRK-CNPJ-AUX-NUM        PIC X(09).
           03  FILLER                  REDEFINES WRK-CNPJ-AUX-NUM.
4S2511*               05  WRK-CORPO-CPF-1     PIC 9(003).
4S2511                05  WRK-CORPO-CPF-1     PIC X(03).
4S2511*               05  WRK-CORPO-CPF-2     PIC 9(003).
4S2511                05  WRK-CORPO-CPF-2     PIC X(03).
4S2511*               05  WRK-CORPO-CPF-3     PIC 9(003).
4S2511                05  WRK-CORPO-CPF-3     PIC X(03).
4S2511*           03  WRK-FILIAL-AUX          PIC 9(004).
4S2511            03  WRK-FILIAL-AUX          PIC X(04).
           03  WRK-CONTROLE-AUX        PIC 9(002).

       01  WRK-CPF-CNPJ-FORMATADO      PIC X(020) VALUE SPACES.

       01  WRK-PC-PARM-REAJS-X.
           05  WRK-PC-PARM-REAJS       PIC ZZZ.ZZ9,9999999 VALUE ZEROS.

       01  WRK-DATA.
           03  WRK-DIA                  PIC  X(02) VALUE ZEROS.
           03  FILLER                   PIC  X(01) VALUE '/'.
           03  WRK-MES                  PIC  X(02) VALUE ZEROS.
           03  FILLER                   PIC  X(01) VALUE '/'.
           03  WRK-ANO                  PIC  X(04) VALUE ZEROS.

       01  WRK-DATA1.
           03  WRK-DIA1                 PIC  X(02) VALUE ZEROS.
           03  WRK-MES1                 PIC  X(02) VALUE ZEROS.
           03  WRK-ANO1                 PIC  X(04) VALUE ZEROS.

FB1219 01  WRK-SPACES                  PIC  X(01)         VALUE SPACES.
 .     01  WRK-PGM-AREA                PIC  X(08)         VALUE SPACES.
 .     01  WRK-ALOCACAO-MEMORIA.
 .         05  WRK-ALOC-DICM           PIC  X(01)         VALUE SPACES.
 .         05  WRK-ALOC-CMFE3JCC       PIC  X(01)         VALUE SPACES.
 .         05  WRK-ALOC-FEMP4A7C       PIC  X(01)         VALUE SPACES.
 .         05  WRK-ALOC-CMFE0010       PIC  X(01)         VALUE SPACES.
 .         05  WRK-ALOC-SBIIC31        PIC  X(01)         VALUE SPACES.
 .         05  WRK-ALOC-FEMP4L1S       PIC  X(01)         VALUE SPACES.
 .         05  WRK-ALOC-FEMP4L9S       PIC  X(01)         VALUE SPACES.
FB1219     05  WRK-ALOC-FEMP3GDC       PIC  X(01)         VALUE SPACES.

       77  FILLER                      PIC X(032)  VALUE
           '* FIM DA WORKING DCOM3Z5O *'.

      *----------------------------------------------------------------
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           COPY DCOMW000.
           COPY DCOMWZ9I.

       01 LNK-AREA-DICM.
          COPY DCOMWDC6.

       01 LNK-AREA-CMFE3JCC.
          COPY CMFEW000.
          COPY CMFEWJCI.

       01 LNK-AREA-FEMP4A7C.
          COPY DCOMW000.
          COPY FEMPWA7O.

       01  LNK-AREA-CMFE0010.
           05  LNK-VALOR-EXTENSO       PIC X(100).
           05  LNK-ROTINA-POOL1060.
               10  LNK-VALOR-1060      PIC X(013).
               10  LNK-VALOR-EXT-1060  PIC X(190).
               10  LNK-FILLER-1060     PIC X(100).

CD1912 01  LNK-AREA-SBIIC31.
CD1912     03 LNK-AREA-COMIC30.
              05 LNK-IMS-HPD2000C.
                 10  LNK-IMS-HPD-BLOCOA.
                     15  LNK-IMS-HPD-TRX            PIC X(4).
                     15  LNK-IMS-HPD-ID             PIC X(3).
                     15  LNK-IMS-HPD-NUM-PERIF      PIC X(8).
                     15  LNK-IMS-HPD-PERIF-ANT      PIC X(1).
                     15  LNK-IMS-HPD-LU-RESPOSTA    PIC 9.
                         88  LNK-IMS-HPD-LU-RESPOSTA-MESMA    VALUE 0.
                         88  LNK-IMS-HPD-LU-RESPOSTA-1        VALUE 1.
                         88  LNK-IMS-HPD-LU-RESPOSTA-2        VALUE 2.
                         88  LNK-IMS-HPD-LU-RESPOSTA-3        VALUE 3.
                         88  LNK-IMS-HPD-LU-RESPOSTA-4        VALUE 4.
                 10  LNK-IMS-HPD-BLOCOB.
                     15  LNK-IMS-HPD-AG-ORIGEM      PIC 9(4).
                     15  LNK-IMS-HPD-SEQ-PERIF      PIC X(6).
                     15  LNK-IMS-HPD-AG-DESTINO     PIC 9(4).
                     15  LNK-IMS-HPD-ANULACAO       PIC X(20).
                     15  LNK-IMS-HPD-COD-PERIF      PIC 9(3).
                     15  LNK-IMS-HPD-CTRLE-TX       PIC X(2).
                     15  LNK-IMS-HPD-RCODE1         PIC X(1).
                     15  LNK-IMS-HPD-RESERVA        PIC X(23).
              05  LNK-IMS-IC30-COM.
                  10  LNK-IMS-IC30-FUNCAO           PIC X(001).
                      88 LNK-IMS-IC30-FUNC-NORM     VALUE 'N'.
                      88 LNK-IMS-IC30-FUNC-NORM2    VALUE SPACES.
                      88 LNK-IMS-IC30-FUNC-ANUL     VALUE 'A'.
                      88 LNK-IMS-IC30-FUNC-VALID    VALUE 'V'.
                      88 LNK-IMS-IC30-FUNC-CONS     VALUE 'C'.
                  10  LNK-IMS-IC30-RCODE REDEFINES LNK-IMS-IC30-FUNCAO.
                      12 FILLER                     PIC X(001).
                         88 LNK-IMS-IC30-RC-OK      VALUE 'O'.
                         88 LNK-IMS-IC30-RC-ERRO    VALUE 'E'.
                         88 LNK-IMS-IC30-RC-INDISP  VALUE 'I'.
                         88 LNK-IMS-IC30-RC-ANUL    VALUE 'A'.
                  10  LNK-IMS-IC30-ID-PERIF         PIC X(03).
                  10  LNK-IMS-IC30-TEXT-LEN         PIC S9(04) COMP.
                  10  LNK-IMS-IC30-RESERVA          PIC X(04).
              05  LNK-IMS-TRANSACAO                 PIC X(09).
              05  LNK-IMS-AREA-EMPF                 PIC X(1608).

CD1912     03 LNK-AREA-COMUNIC.
              05  LNK-MOD-IMS                       PIC X(0008).
              05  LNK-AREA-MOD-IMS                  PIC X(1600).

CD1912 01  LNK-AREA-FEMP4L1S.
           COPY DCOMW000.
           COPY FEMPWL1C.

CD1912 01  LNK-AREA-FEMP4L9S.
           COPY 'DCOMW000'.
           COPY 'FEMPWL9C'.

CD1912 01  LNK-AREA-FEMP3GDC.
           COPY 'DCOMW000'.
           COPY 'FEMPWGDI'.

      *================================================================*
       PROCEDURE                       DIVISION  USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 8800-ALOCAR-DICM.

           PERFORM 2200-MONTAR-DADOS-CONTRATO
           PERFORM 2250-MONTAR-CLIENTE
           PERFORM 2350-MONTAR-PARCELA

           MOVE ZEROS                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
           .
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-MONTAR-DADOS-CONTRATO      SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMW91I-S-AGENCIA     TO WDCM-AG-CONTRANTE

      *    MOV3 DCOMW86I-S-DIGITO-AGENCIA
      *                                TOO WRK-9DE1
      *    MOV3 WRK-XDE1               TOO DCOMW86I-S-DIGITO-AGENCIA

           MOVE DCOMW91I-S-CONTA-CORRENTE
                                       TO WDCM-CTA-CONTRANTE
           MOVE DCOMW91I-S-DIGITO-CONTA
                                       TO WRK-9DE1
           MOVE WRK-XDE1               TO WDCM-DIG-CTA-CONTRANTE

V.001      MOVE DCOMW91I-S-DATA-OPERACAO
.                                      TO WRK-DATA-OPERACAO
V.001      MOVE WRK-DATA-OPERACAO-RED  TO WRK-DATA1
           MOVE WRK-DIA1               TO WRK-DIA
           MOVE WRK-MES1               TO WRK-MES
           MOVE WRK-ANO1               TO WRK-ANO
           MOVE WRK-DATA               TO WDCM-DT-EMITT-CONTR-CDULA

           MOVE DCOMW91I-S-NR-OPERACAO
                                       TO WDCM-NU-DOCTO

           MOVE DCOMW91I-S-ENDERECO-CLIENTE
                                       TO WDCM-END-EMITENTE
           MOVE DCOMW91I-S-CIDADE-CLIENTE
                                       TO WDCM-CIDADE-EMITENTE
           MOVE DCOMW91I-S-ESTADO-CLIENTE
                                       TO WDCM-UF-EMITENTE
           MOVE DCOMW91I-S-AGENCIA     TO WDCM-AG-EMITENTE
           MOVE DCOMW91I-S-NOME-AGENCIA
                                       TO WDCM-NOME-AG-EMITENTE
           MOVE DCOMW91I-S-CONTA-CORRENTE
                                       TO WDCM-CTA-EMITENTE
           MOVE DCOMW91I-S-DIGITO-CONTA
                                       TO WRK-9DE1
           MOVE WRK-XDE1               TO WDCM-DIG-CTA-EMITENTE

           MOVE DCOMW91I-S-VLR-TIT-AT
                                       TO WDCM-VR-PRINC-CONTR

      *    MOV3 DCOMW86I-NR-VIAS       TOO WDCM-NR-VIAS
           .
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-MONTAR-CLIENTE             SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMW91I-S-NOME-CLIENTE
                                       TO WDCM-NOME-EMITENTE

           MOVE DCOMW91I-S-CPF-CLIENTE
                                       TO WRK-CNPJ-AUX-NUM
           MOVE DCOMW91I-S-FILIAL-CLIENTE
                                       TO WRK-FILIAL-AUX
           MOVE DCOMW91I-S-CTRL-CLIENTE
                                       TO WRK-CONTROLE-AUX

           PERFORM 5000-FORMATAR-CPF-CNPJ.

           MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF
                                          WDCM-CPF-EMITENTE.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       2350-MONTAR-PARCELA             SECTION.
      *----------------------------------------------------------------*

               MOVE  DCOMW91I-S-NUM-TITULO        TO
                                           WDCM-NU-TITULO
               MOVE  DCOMW91I-S-COD-CONTRATO TO
                                           WDCM-NU-CONTRATO
V.001          MOVE DCOMW91I-S-DATA-VENC-ORIG
.                                          TO WRK-DATA-OPERACAO
V.001          MOVE WRK-DATA-OPERACAO-RED  TO WRK-DATA1
               MOVE WRK-DIA1               TO WRK-DIA
               MOVE WRK-MES1               TO WRK-MES
               MOVE WRK-ANO1               TO WRK-ANO
               MOVE WRK-DATA               TO
                                           WDCM-DT-CONTRATO
           IF DCOMW91I-S-VLR-TIT-AT        NOT EQUAL
                                           DCOMW91I-S-VLT-TIT-NV
               MOVE  DCOMW91I-S-VLR-TIT-AT TO
                                           WDCM-VR-TITULO-DE
               MOVE  DCOMW91I-S-VLT-TIT-NV TO
                                           WDCM-VR-TITULO-PARA
           ELSE
               MOVE  ZEROS                 TO WDCM-VR-TITULO-DE
                                              WDCM-VR-TITULO-PARA
           END-IF.

           IF DCOMW91I-S-VLR-TIT-EXT-AT    NOT EQUAL
                                           DCOMW91I-S-VLR-TIT-EXT-NV
               MOVE  DCOMW91I-S-VLR-TIT-EXT-AT        TO
                                           WDCM-VR-TITULOS-EXTENSO-DE
               MOVE  DCOMW91I-S-VLR-TIT-EXT-NV        TO
                                           WDCM-VR-TITULOS-EXTENSO-PARA
           ELSE
               MOVE  SPACES                TO WDCM-VR-TITULOS-EXTENSO-DE
                                           WDCM-VR-TITULOS-EXTENSO-PARA
           END-IF.

           IF DCOMW91I-S-NOME-SAC-AT       NOT EQUAL
                                           DCOMW91I-S-NOME-SACADO
               MOVE  DCOMW91I-S-NOME-SAC-AT TO
                                           WDCM-NOME-DE
               MOVE  DCOMW91I-S-NOME-SACADO TO
                                           WDCM-NOME-PARA
           ELSE
               MOVE SPACES                 TO WDCM-NOME-DE
                                              WDCM-NOME-PARA
           END-IF.

           IF DCOMW91I-S-ENDERC-SAC-AT     NOT EQUAL
                                           DCOMW91I-S-ENDERC-SAC-NV
               MOVE  DCOMW91I-S-ENDERC-SAC-AT TO
                                           WDCM-END-DE
               MOVE  DCOMW91I-S-ENDERC-SAC-NV TO
                                           WDCM-END-PARA
           ELSE
               MOVE SPACES                 TO WDCM-END-DE
                                              WDCM-END-PARA
           END-IF.

           IF DCOMW91I-S-CNPJ-CPF-SAC-AT   NOT EQUAL
                                           DCOMW91I-S-CNPJ-CPF-SAC-NV
               MOVE DCOMW91I-S-CNPJ-CPF-SAC-AT
                                       TO WRK-CNPJ-AUX-NUM
               MOVE DCOMW91I-S-FILIAL-SAC-AT
                                       TO WRK-FILIAL-AUX
               MOVE DCOMW91I-S-CTRL-SAC-AT
                                       TO WRK-CONTROLE-AUX
               PERFORM 5000-FORMATAR-CPF-CNPJ

               MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF-CNPJ-PARA

               MOVE DCOMW91I-S-CNPJ-CPF-SAC-NV
                                       TO WRK-CNPJ-AUX-NUM
               MOVE DCOMW91I-S-FILIAL-SAC-N
                                       TO WRK-FILIAL-AUX
               MOVE DCOMW91I-S-CTRL-SAC-N
                                       TO WRK-CONTROLE-AUX
               PERFORM 5000-FORMATAR-CPF-CNPJ

               MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF-CNPJ-DE
           ELSE
               MOVE SPACES             TO WDCM-CPF-CNPJ-DE
                                          WDCM-CPF-CNPJ-PARA
           END-IF.

           IF DCOMW91I-S-TAX-JUROS-AM-AT NOT EQUAL
                                         DCOMW91I-S-TAX-JUROS-AM-NV
               MOVE DCOMW91I-S-TAX-JUROS-AM-AT
                                       TO WRK-PC-PARM-REAJS
               MOVE WRK-PC-PARM-REAJS-X
                                       TO WDCM-TX-JURO-EFE-MES-DE
               MOVE DCOMW91I-S-TAX-JUROS-AM-NV
                                       TO WRK-PC-PARM-REAJS
               MOVE WRK-PC-PARM-REAJS-X
                                       TO WDCM-TX-JURO-EFE-MES-PARA
           ELSE
               MOVE SPACES             TO WDCM-TX-JURO-EFE-MES-DE
                                          WDCM-TX-JURO-EFE-MES-PARA
           END-IF.

           IF DCOMW91I-S-TAX-JUROS-AA-AT NOT EQUAL
                                         DCOMW91I-S-TAX-JUROS-AA-NV
               MOVE DCOMW91I-S-TAX-JUROS-AA-AT
                                       TO WRK-PC-PARM-REAJS
               MOVE WRK-PC-PARM-REAJS-X
                                       TO WDCM-TX-JURO-EFE-ANO-DE
               MOVE DCOMW91I-S-TAX-JUROS-AA-NV
                                       TO WRK-PC-PARM-REAJS
               MOVE WRK-PC-PARM-REAJS-X
                                       TO WDCM-TX-JURO-EFE-ANO-PARA
           ELSE
               MOVE SPACES             TO WDCM-TX-JURO-EFE-ANO-DE
                                          WDCM-TX-JURO-EFE-ANO-PARA
           END-IF.

           IF  DCOMW91I-S-NOME-AVAL-AT   NOT EQUAL
                                         DCOMW91I-S-NOME-AVAL-NV
               MOVE DCOMW91I-S-NOME-AVAL-AT
                                       TO WDCM-NOME-SACADOR-PARA
               MOVE DCOMW91I-S-NOME-AVAL-NV
                                       TO WDCM-NOME-SACADOR-DE
           ELSE
               MOVE SPACES             TO WDCM-NOME-SACADOR-DE
                                          WDCM-NOME-SACADOR-PARA
           END-IF.

           IF  DCOMW91I-S-CNPJ-AVAL-AT    NOT EQUAL
                                          DCOMW91I-S-CNPJ-AVAL-NV
               MOVE DCOMW91I-S-CNPJ-AVAL-AT
                                       TO WRK-CNPJ-AUX-NUM
               MOVE DCOMW91I-S-FILIAL-AVAL-AT
                                       TO WRK-FILIAL-AUX
               MOVE DCOMW91I-S-CTRL-AVAL-AT
                                       TO WRK-CONTROLE-AUX
               PERFORM 5000-FORMATAR-CPF-CNPJ

               MOVE WRK-CPF-CNPJ-FORMATADO
                                       TO WDCM-CPF-CNPJ-SACADOR-PARA
               MOVE DCOMW91I-S-CNPJ-AVAL-NV
                                       TO WRK-CNPJ-AUX-NUM
               MOVE DCOMW91I-S-FILIAL-AVAL-NV
                                       TO WRK-FILIAL-AUX
               MOVE DCOMW91I-S-CTRL-AVAL-NV
                                       TO WRK-CONTROLE-AUX
               PERFORM 5000-FORMATAR-CPF-CNPJ

               MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF-CNPJ-SACADOR-DE
           ELSE
               MOVE SPACES                 TO WDCM-CPF-CNPJ-SACADOR-DE
                                              WDCM-CPF-CNPJ-SACADOR-PARA
           END-IF

           IF DCOMW91I-S-DATA-VENC-ORIG    NOT EQUAL
                                           DCOMW91I-S-DATA-VENC-PRRG
V.001          MOVE DCOMW91I-S-DATA-VENC-ORIG
.                                          TO WRK-DATA-OPERACAO
V.001          MOVE WRK-DATA-OPERACAO-RED  TO WRK-DATA1
               MOVE WRK-DIA1               TO WRK-DIA
               MOVE WRK-MES1               TO WRK-MES
               MOVE WRK-ANO1               TO WRK-ANO
               MOVE WRK-DATA               TO
                                           WDCM-DT-VENC-DE
V.001          MOVE DCOMW91I-S-DATA-VENC-PRRG
.                                          TO WRK-DATA-VENC-PRRG
V.001          MOVE WRK-DATA-VENC-PRRG-RED TO WRK-DATA1
               MOVE WRK-DIA1               TO WRK-DIA
               MOVE WRK-MES1               TO WRK-MES
               MOVE WRK-ANO1               TO WRK-ANO
               MOVE WRK-DATA               TO
                                           WDCM-DT-VENC-PARA
           ELSE
               MOVE SPACES                 TO WDCM-DT-VENC-DE
                                              WDCM-DT-VENC-PARA
           END-IF.
               .

      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

      *--  AJUSTE PARA EVITAR S0C4 CASO ALGUM MODULO DE ERRO
      *--  ANTES DE ALOCAR A AREA DO DICM
           IF  WRK-ALOC-DICM NOT EQUAL 'S'
               PERFORM 8800-ALOCAR-DICM
           END-IF.

           MOVE LNK-AREA-DICM          TO DCOMWZ9I-S-AREA-DICM.

CD1912     MOVE    '*'                 TO WRK-PGM-AREA.
CD1912     PERFORM 8600-LIBERAR-MEMORIA.

           EXEC CICS
                RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-FORMATAR-CPF-CNPJ          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-CPF-CNPJ-FORMATADO.

4S2511*    IF  WRK-FILIAL-AUX NOT EQUAL ZEROS
4S2511     IF  WRK-FILIAL-AUX EQUAL SPACES OR
4S2511         WRK-FILIAL-AUX EQUAL LOW-VALUES
4S2511         STRING WRK-CORPO-CPF-1 '.'
4S2511                WRK-CORPO-CPF-2 '.'
4S2511                WRK-CORPO-CPF-3 '-'
4S2511                WRK-CONTROLE-AUX
4S2511         DELIMITED BY SIZE     INTO WRK-CPF-CNPJ-FORMATADO
4S2511     ELSE
4S2511         STRING WRK-CORPO-CPF-1 '.'
4S2511                WRK-CORPO-CPF-2 '.'
4S2511                WRK-CORPO-CPF-3 '/'
4S2511                WRK-FILIAL-AUX  '-'
4S2511                WRK-CONTROLE-AUX
4S2511         DELIMITED BY SIZE     INTO WRK-CPF-CNPJ-FORMATADO
4S2511     END-IF.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

CD1912*----------------------------------------------------------------*
 .     8500-ALOCAR-MEMORIA             SECTION.
 .    *----------------------------------------------------------------*
 .
           EVALUATE WRK-PGM-AREA
              WHEN 'DICM'
                IF  WRK-ALOC-DICM     NOT EQUAL 'S'
                    EXEC CICS GETMAIN
                         SET (ADDRESS OF LNK-AREA-DICM)
                         FLENGTH (LENGTH OF LNK-AREA-DICM)
                         INITIMG (WRK-SPACES)
                    END-EXEC
                    MOVE 'S'           TO WRK-ALOC-DICM
                END-IF

              WHEN 'CMFE3JCC'
                IF  WRK-ALOC-CMFE3JCC NOT EQUAL 'S'
                    EXEC CICS GETMAIN
                         SET (ADDRESS OF LNK-AREA-CMFE3JCC)
                         FLENGTH (LENGTH OF LNK-AREA-CMFE3JCC)
                         INITIMG (WRK-SPACES)
                    END-EXEC
                    MOVE 'S'           TO WRK-ALOC-CMFE3JCC
                END-IF

              WHEN 'FEMP4A7C'
                IF  WRK-ALOC-FEMP4A7C NOT EQUAL 'S'
                    EXEC CICS GETMAIN
                         SET (ADDRESS OF LNK-AREA-FEMP4A7C)
                         FLENGTH (LENGTH OF LNK-AREA-FEMP4A7C)
                         INITIMG (WRK-SPACES)
                    END-EXEC
                    MOVE 'S'           TO WRK-ALOC-FEMP4A7C
                END-IF

              WHEN 'CMFE0010'
                IF  WRK-ALOC-CMFE0010 NOT EQUAL 'S'
                    EXEC CICS GETMAIN
                         SET (ADDRESS OF LNK-AREA-CMFE0010)
                         FLENGTH (LENGTH OF LNK-AREA-CMFE0010)
                         INITIMG (WRK-SPACES)
                    END-EXEC
                    MOVE 'S'           TO WRK-ALOC-CMFE0010
                END-IF

              WHEN 'SBIIC31'
                IF  WRK-ALOC-SBIIC31  NOT EQUAL 'S'
                    EXEC CICS GETMAIN
                         SET (ADDRESS OF LNK-AREA-SBIIC31)
                         FLENGTH (LENGTH OF LNK-AREA-SBIIC31)
                         INITIMG (WRK-SPACES)
                    END-EXEC
                    MOVE 'S'           TO WRK-ALOC-SBIIC31
                END-IF

              WHEN 'FEMP4L1S'
                IF  WRK-ALOC-FEMP4L1S NOT EQUAL 'S'
                    EXEC CICS GETMAIN
                         SET (ADDRESS OF LNK-AREA-FEMP4L1S)
                         FLENGTH (LENGTH OF LNK-AREA-FEMP4L1S)
                         INITIMG (WRK-SPACES)
                    END-EXEC
                    MOVE 'S'           TO WRK-ALOC-FEMP4L1S
                END-IF

              WHEN 'FEMP4L9S'
                IF  WRK-ALOC-FEMP4L9S NOT EQUAL 'S'
                    EXEC CICS GETMAIN
                         SET (ADDRESS OF LNK-AREA-FEMP4L9S)
                         FLENGTH (LENGTH OF LNK-AREA-FEMP4L9S)
                         INITIMG (WRK-SPACES)
                    END-EXEC
                    MOVE 'S'           TO WRK-ALOC-FEMP4L9S
                END-IF

              WHEN 'FEMP3GDC'
                IF  WRK-ALOC-FEMP3GDC NOT EQUAL 'S'
                    EXEC CICS GETMAIN
                         SET (ADDRESS OF LNK-AREA-FEMP3GDC)
                         FLENGTH (LENGTH OF LNK-AREA-FEMP3GDC)
                         INITIMG (WRK-SPACES)
                    END-EXEC
                    MOVE 'S'           TO WRK-ALOC-FEMP3GDC
                END-IF

           END-EVALUATE.
 .
 .    *----------------------------------------------------------------*
 .     8500-99-FIM.                    EXIT.
CD1912*----------------------------------------------------------------*

CD1912*----------------------------------------------------------------*
 .     8600-LIBERAR-MEMORIA            SECTION.
 .    *----------------------------------------------------------------*
 .
           IF  WRK-ALOC-DICM EQUAL 'S'
               IF  WRK-PGM-AREA EQUAL 'DICM' OR '*'
                   EXEC CICS FREEMAIN
                        DATA (LNK-AREA-DICM)
                   END-EXEC

                   MOVE 'N'            TO WRK-ALOC-DICM
               END-IF
           END-IF.

           IF  WRK-ALOC-CMFE3JCC EQUAL 'S'
               IF  WRK-PGM-AREA EQUAL 'CMFE3JCC' OR '*'
                   EXEC CICS FREEMAIN
                        DATA (LNK-AREA-CMFE3JCC)
                   END-EXEC

                   MOVE 'N'            TO WRK-ALOC-CMFE3JCC
               END-IF
           END-IF.

           IF  WRK-ALOC-FEMP4A7C EQUAL 'S'
               IF  WRK-PGM-AREA EQUAL 'FEMP4A7C' OR '*'
                   EXEC CICS FREEMAIN
                        DATA (LNK-AREA-FEMP4A7C)
                   END-EXEC

                   MOVE 'N'            TO WRK-ALOC-FEMP4A7C
               END-IF
           END-IF.

           IF  WRK-ALOC-CMFE0010 EQUAL 'S'
               IF  WRK-PGM-AREA EQUAL 'CMFE0010' OR '*'
                   EXEC CICS FREEMAIN
                        DATA (LNK-AREA-CMFE0010)
                   END-EXEC

                   MOVE 'N'            TO WRK-ALOC-CMFE0010
               END-IF
           END-IF.

           IF  WRK-ALOC-SBIIC31 EQUAL 'S'
               IF  WRK-PGM-AREA EQUAL 'SBIIC31' OR '*'
                   EXEC CICS FREEMAIN
                        DATA (LNK-AREA-SBIIC31)
                   END-EXEC

                   MOVE 'N'            TO WRK-ALOC-SBIIC31
               END-IF
           END-IF.

           IF  WRK-ALOC-FEMP4L1S EQUAL 'S'
               IF  WRK-PGM-AREA EQUAL 'FEMP4L1S' OR '*'
                   EXEC CICS FREEMAIN
                        DATA (LNK-AREA-FEMP4L1S)
                   END-EXEC

                   MOVE 'N'            TO WRK-ALOC-FEMP4L1S
               END-IF
           END-IF.

           IF  WRK-ALOC-FEMP4L9S EQUAL 'S'
               IF  WRK-PGM-AREA EQUAL 'FEMP4L9S' OR '*'
                   EXEC CICS FREEMAIN
                        DATA (LNK-AREA-FEMP4L9S)
                   END-EXEC

                   MOVE 'N'            TO WRK-ALOC-FEMP4L9S
               END-IF
           END-IF.

           IF  WRK-ALOC-FEMP3GDC EQUAL 'S'
               IF  WRK-PGM-AREA EQUAL 'FEMP3GDC' OR '*'
                   EXEC CICS FREEMAIN
                        DATA (LNK-AREA-FEMP3GDC)
                   END-EXEC

                   MOVE 'N'            TO WRK-ALOC-FEMP3GDC
               END-IF
           END-IF.
 .
 .    *----------------------------------------------------------------*
 .     8600-99-FIM.                    EXIT.
CD1912*----------------------------------------------------------------*

CD1912*----------------------------------------------------------------*
 .     8800-ALOCAR-DICM                SECTION.
 .    *----------------------------------------------------------------*

CD1912     MOVE 'DICM'                 TO WRK-PGM-AREA
CD1912     PERFORM 8500-ALOCAR-MEMORIA.
CD1912*    INITIALIZEEE LNK-AREA-DICM.

 .    *----------------------------------------------------------------*
 .     8800-99-FIM.                    EXIT.
CD1912*----------------------------------------------------------------*
