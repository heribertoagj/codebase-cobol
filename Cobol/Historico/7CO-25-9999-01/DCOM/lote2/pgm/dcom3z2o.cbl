*================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM3Z2O
      *================================================================*
      *                      F O U R S Y S                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....: DCOM3Z2O                                      *
      *    ANALISTA....: RODRIGO PEREIRA                               *
      *    DATA........: 26/12/2022                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: FORMATAR LAYOUT DICM PARA IMPRESSAO DE        *
      *                  FORMULARIOS NO DOCG                           *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS:                                           *
      *    DCOMW000 BOOK BLOCO DE CONTROLE CHAMADA SERVICOS FUNCIONAIS *
      *    FEMPWZ4I AREA DE COMUNICACAO                                *
      *----------------------------------------------------------------*
      *    BOOKS ARQUITETURAIS:                                        *
      *    I#FRWKMD COMMAREA FRWK1999 (LOG DE ERROS MODULO)            *
      *----------------------------------------------------------------*
      *    MODULOS  - DESCRICAO                                        *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
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
           '* INICIO DA WORKING DCOM3Z2O *'.

       01  IND-1                       PIC 9(001)  COMP-3 VALUE ZEROS.

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

       77  FILLER                      PIC X(032)  VALUE
           '* FIM DA WORKING DCOM3Z2O *'.

      *----------------------------------------------------------------
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA.
           COPY DCOMW000.
           COPY DCOMWZ6I.


CD1912 01 LNK-AREA-DICM.
          COPY DCOMWDC5.

CD1912 01 LNK-AREA-CMFE3JCC.
          COPY CMFEW000.
          COPY CMFEWJCI.

CD1912 01 LNK-AREA-FEMP4A7C.
          COPY FEMPWA7O.

CD1912 01  LNK-AREA-CMFE0010.
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
           COPY FEMPWL1C.

CD1912 01  LNK-AREA-FEMP4L9S.
           COPY 'FEMPWL9C'.

CD1912 01  LNK-AREA-FEMP3GDC.
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

      *    SETT WRK-POINTER-DCOMW90I    TOO ADDRESS
      *                                OOF FEMPWZ4I-E-CMFIWICI.
      *
      *    SETT ADDRESS OOF LNK-AREA-DCOMW90I
      *                                TOO WRK-POINTER-DCOMW90I.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 8800-ALOCAR-DICM.

           PERFORM 2200-MONTAR-DADOS-CONTRATO
           PERFORM 2250-MONTAR-CLIENTE
           PERFORM 2350-MONTAR-FIADOR
           PERFORM 2370-MONTAR-AVALISTA
           PERFORM 2380-MONTAR-ASSINATURA

           MOVE ZEROS                  TO DCOMW000-COD-RETORNO
                                       OF DFHCOMMAREA
           .
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-MONTAR-DADOS-CONTRATO      SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMW90I-S-AGENCIA-AT
                                       TO WDCM-AG-CONTRANTE

           MOVE DCOMW90I-S-DIGITO-AGENCIA-AT
                                       TO WRK-9DE1
           MOVE WRK-XDE1               TO WDCM-DIG-AG-CONTRANTE

           MOVE DCOMW90I-S-CONTA-CORRENTE-AT
                                       TO WDCM-CTA-CONTRANTE
           MOVE DCOMW90I-S-DIGITO-CONTA-AT
                                       TO WRK-9DE1
           MOVE WRK-XDE1               TO WDCM-DIG-CTA-CONTRANTE

           MOVE DCOMW90I-S-DATA-OPERACAO
                                       TO WRK-DATA1
           MOVE WRK-DIA1               TO WRK-DIA
           MOVE WRK-MES1               TO WRK-MES
           MOVE WRK-ANO1               TO WRK-ANO
           MOVE WRK-DATA
                                       TO WDCM-DT-EMITT-CONTR-CDULA

           MOVE DCOMW90I-S-NR-OPERACAO
                                       TO WDCM-NU-DOCTO

           MOVE DCOMW90I-S-VALOR-OPERACAO-AT
                                       TO WDCM-VR-PRINC-CONTR


           MOVE DCOMW90I-S-CNPJ
                                       TO WRK-CNPJ-AUX-NUM
           MOVE DCOMW90I-S-FILIAL
                                       TO WRK-FILIAL-AUX
           MOVE DCOMW90I-S-CTRL
                                       TO WRK-CONTROLE-AUX

           PERFORM 5000-FORMATAR-CPF-CNPJ.

           MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF-EMITENTE

      *    MOV3 DCOMW90I-NR-VIAS       TOO WDCM-NR-VIAS
           .
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2250-MONTAR-CLIENTE             SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMW90I-S-NOME-CLIENTE
                                       TO WDCM-NOME-EMITENTE

           MOVE DCOMW90I-S-CNPJ
                                       TO WRK-CNPJ-AUX-NUM
           MOVE DCOMW90I-S-FILIAL
                                       TO WRK-FILIAL-AUX
           MOVE DCOMW90I-S-CTRL
                                       TO WRK-CONTROLE-AUX

           PERFORM 5000-FORMATAR-CPF-CNPJ.

           MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF

           MOVE DCOMW90I-S-ENDERECO
                                       TO WDCM-END-EMITENTE

           MOVE DCOMW90I-S-ENDERECO
                                       TO WDCM-END-EMITENTE

           MOVE DCOMW90I-S-CIDADE
                                       TO
                                          WDCM-CIDADE-EMITENTE
           MOVE DCOMW90I-S-ESTADO
                                       TO
                                          WDCM-UF-EMITENTE

      *    SSSTRING DCOMW90I-CCEP-TMDOR '-'
      *           DCOMW90I-CCEP-COMPL-TMDOR
      *    DDDELIMITED BY SIZE           IIINTO WDCM-CEP-EMITENTE

           MOVE DCOMW90I-S-DIGITO-AGENCIA
                                       TO WDCM-DIG-AG-EMITENTE

           MOVE DCOMW90I-S-DATA-OPERACAO
                                       TO WRK-DATA1
           MOVE WRK-DIA1               TO WRK-DIA
           MOVE WRK-MES1               TO WRK-MES
           MOVE WRK-ANO1               TO WRK-ANO
           MOVE WRK-DATA
                                       TO WDCM-DT-BORDERO

           MOVE DCOMW90I-S-NOME-AGENCIA
                                       TO WDCM-NOME-AG-EMITENTE.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       2350-MONTAR-FIADOR              SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMW90I-S-AGENCIA     TO
                                          WDCM-AG-BORDERO

           MOVE DCOMW90I-S-DIGITO-AGENCIA
                                       TO WRK-9DE1
           MOVE WRK-XDE1               TO
                                          WDCM-DIG-AG-EMITENTE

           MOVE DCOMW90I-S-CONTA-CORRENTE
                                       TO
                                          WDCM-CTA-BORDERO
           MOVE DCOMW90I-S-DIGITO-CONTA
                                       TO WRK-9DE1
           MOVE WRK-XDE1               TO
                                          WDCM-DIG-CTA

           MOVE DCOMW90I-S-AGENCIA-AT  TO WDCM-AG-BORDERO-DE

      *    MOV3 DCOMW90I-S-DIGITO-AGENCIA
      *                                T WRK-9DE1
      *    MOV3 WRK-XDE1               T WDCM-DIG-AG-EMITENTE


           MOVE DCOMW90I-S-CONTA-CORRENTE-AT
                                       TO WDCM-CTA-BORDERO-DE

           MOVE DCOMW90I-S-DIGITO-CONTA
                                       TO WRK-9DE1
           MOVE WRK-XDE1               TO WDCM-DIG-CTA


           MOVE DCOMW90I-S-VL-CONTRATO-EXTENSO
                                       TO WDCM-VR-TITULOS-EXTENSO
           MOVE DCOMW90I-S-VALOR-LIMITE
                                       TO WDCM-VR-TITULO

           MOVE DCOMW90I-S-VALOR-OPERACAO-AT
                                       TO WDCM-VR-TITULO-DE
           MOVE DCOMW90I-S-VL-OPERACAO-EXTENSO
                                       TO
                                          WDCM-VR-TITULOS-EXTENSO-DE
           MOVE DCOMW90I-S-VL-NT-PROMISSORIA
                                       TO WDCM-VR-NT-PROMISSORIA1
           MOVE DCOMW90I-S-VL-NT-PROMI-EXTENSO
                                       TO WDCM-VR-NT-PROMISSORIA-EXTENSO
           MOVE DCOMW90I-S-JUROS-MES
                                       TO WDCM-TX-JURO-EFETIVO-MES
           MOVE DCOMW90I-S-JUROS-ANO
                                       TO WDCM-TX-JURO-EFETIVO-ANO
           MOVE DCOMW90I-S-JUROS-MES-AT
                                       TO WDCM-TX-JURO-EFETIVO-MES-DE
           MOVE DCOMW90I-S-JUROS-ANO-AT
                                       TO WDCM-TX-JURO-EFETIVO-ANO-DE
           MOVE DCOMW90I-S-NOME-AVAL-1AT
                                       TO WDCM-NOME-AVAL-DE
           MOVE DCOMW90I-S-NOME-AVAL-2AT
                                       TO WDCM-NOME-AVAL-DE1

           MOVE DCOMW90I-S-DATA-VENCIMENTO
                                       TO WRK-DATA1
           MOVE WRK-DIA1               TO WRK-DIA
           MOVE WRK-MES1               TO WRK-MES
           MOVE WRK-ANO1               TO WRK-ANO
           MOVE WRK-DATA
                                       TO WDCM-DT-VENCIMENTO

           MOVE DCOMW90I-S-DT-VENCIMENTO-AT
                                       TO WRK-DATA1
           MOVE WRK-DIA1               TO WRK-DIA
           MOVE WRK-MES1               TO WRK-MES
           MOVE WRK-ANO1               TO WRK-ANO
           MOVE WRK-DATA               TO WDCM-DT-ALTERADA-DE.

      *----------------------------------------------------------------*
       2350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2370-MONTAR-AVALISTA            SECTION.
      *----------------------------------------------------------------*

           MOVE 1                      TO IND-1
           MOVE DCOMW90I-S-NOME-AVAL-1AT
                                       TO WDCM-NOME-AVAL(IND-1)
4S2511*           IF DCOMW90I-S-CNPJ-AVAL-1AT NOT EQUAL ZEROS
4S2511            IF DCOMW90I-S-CNPJ-AVAL-1AT EQUAL SPACES OR
4S2511               DCOMW90I-S-CNPJ-AVAL-1AT EQUAL LOW-VALUES
               MOVE SPACES              TO WDCM-CPF-CNPJ-AVAL(IND-1)
           ELSE
               MOVE DCOMW90I-S-CNPJ-AVAL-1AT
                                       TO WRK-CNPJ-AUX-NUM
              MOVE DCOMW90I-S-FILIAL-AVAL-1AT
                                       TO WRK-FILIAL-AUX
              MOVE DCOMW90I-S-CTRL-AVAL-1AT
                                       TO WRK-CONTROLE-AUX
              PERFORM 5000-FORMATAR-CPF-CNPJ
              MOVE WRK-CPF-CNPJ-FORMATADO
                                       TO WDCM-CPF-CNPJ-AVAL(IND-1)
           END-IF

           MOVE DCOMW90I-S-ENDERECO-AVAL-1AT
                                       TO WDCM-END-RES-AVAL(IND-1)
           MOVE DCOMW90I-S-CIDADE-AVAL-1AT
                                       TO WDCM-NM-CIDADE-AVAL(IND-1)
           MOVE DCOMW90I-S-ESTADO-AVAL-1AT
                                       TO WDCM-SIGLA-UF-AVAL(IND-1)
           MOVE 2                      TO IND-1
           MOVE DCOMW90I-S-NOME-AVAL-2AT
                                       TO WDCM-NOME-AVAL(IND-1)
4S2511*    IF DCOMW90I-S-CNPJ-AVAL-2AT NOT EQUAL ZEROS
4S2511     IF DCOMW90I-S-CNPJ-AVAL-2AT EQUAL SPACES OR
4S2511        DCOMW90I-S-CNPJ-AVAL-2AT EQUAL LOW-VALUES
              MOVE SPACES              TO WDCM-CPF-CNPJ-AVAL(IND-1)
           ELSE
               MOVE DCOMW90I-S-CNPJ-AVAL-2AT
                                       TO WRK-CNPJ-AUX-NUM
              MOVE DCOMW90I-S-FILIAL-AVAL-2AT
                                       TO WRK-FILIAL-AUX
              MOVE DCOMW90I-S-CTRL-AVAL-2AT
                                       TO WRK-CONTROLE-AUX
              PERFORM 5000-FORMATAR-CPF-CNPJ
              MOVE WRK-CPF-CNPJ-FORMATADO
                                       TO WDCM-CPF-CNPJ-AVAL(IND-1)
           END-IF

           MOVE DCOMW90I-S-ENDERECO-AVAL-2AT
                                       TO WDCM-END-RES-AVAL(IND-1)
           MOVE DCOMW90I-S-CIDADE-AVAL-2AT
                                       TO WDCM-NM-CIDADE-AVAL(IND-1)
           MOVE DCOMW90I-S-ESTADO-AVAL-2AT
                                       TO WDCM-SIGLA-UF-AVAL(IND-1)

           MOVE 1                      TO IND-1
           MOVE DCOMW90I-S-NOME-AVAL-1NV
                                       TO WDCM-NOME-AVAL-NOVO(IND-1)
                                          WDCM-NOME-AVAL-ASSINA1
4S2511*    IF DCOMW90I-S-CNPJ-AVAL-1NV NOT EQUAL ZEROS
4S2511     IF DCOMW90I-S-CNPJ-AVAL-1NV EQUAL SPACES OR
4S2511        DCOMW90I-S-CNPJ-AVAL-1NV EQUAL LOW-VALUES
               MOVE SPACES              TO WDCM-CPF-CNPJ-AVAL-NOVO(IND-1)
                                          WDCM-CPF-CNPJ-AVAL-ASSINA1
           ELSE
               MOVE DCOMW90I-S-CNPJ-AVAL-1NV
                                       TO WRK-CNPJ-AUX-NUM
              MOVE DCOMW90I-S-FILIAL-AVAL-1NV
                                       TO WRK-FILIAL-AUX
              MOVE DCOMW90I-S-CTRL-AVAL-1NV
                                       TO WRK-CONTROLE-AUX
              PERFORM 5000-FORMATAR-CPF-CNPJ
              MOVE WRK-CPF-CNPJ-FORMATADO
                                       TO WDCM-CPF-CNPJ-AVAL-NOVO(IND-1)
                                          WDCM-CPF-CNPJ-AVAL-ASSINA1
           END-IF

           MOVE DCOMW90I-S-ENDERECO-AVAL-1NV
                                       TO WDCM-END-RES-AVAL-NOVO(IND-1)
           MOVE DCOMW90I-S-CIDADE-AVAL-1NV
                                       TO WDCM-NM-CIDADE-AVAL-NOVO
                                          (IND-1)
           MOVE DCOMW90I-S-ESTADO-AVAL-1NV
                                       TO WDCM-SIGLA-UF-AVAL-NOVO(IND-1)
           MOVE 2                      TO IND-1
           MOVE DCOMW90I-S-NOME-AVAL-2NV
                                       TO WDCM-NOME-AVAL-NOVO(IND-1)
                                          WDCM-NOME-AVAL-ASSINA2
4S2511*    IF DCOMW90I-S-CNPJ-AVAL-2NV NOT EQUAL ZEROS
4S2511     IF DCOMW90I-S-CNPJ-AVAL-2NV EQUAL SPACES OR
4S2511        DCOMW90I-S-CNPJ-AVAL-2NV EQUAL LOW-VALUES
              MOVE SPACES              TO WDCM-CPF-CNPJ-AVAL-NOVO(IND-1)
                                          WDCM-CPF-CNPJ-AVAL-ASSINA2
           ELSE
              MOVE DCOMW90I-S-CNPJ-AVAL-2NV
                                       TO WRK-CNPJ-AUX-NUM
              MOVE DCOMW90I-S-FILIAL-AVAL-2NV
                                       TO WRK-FILIAL-AUX
              MOVE DCOMW90I-S-CTRL-AVAL-2NV
                                       TO WRK-CONTROLE-AUX
              PERFORM 5000-FORMATAR-CPF-CNPJ
              MOVE WRK-CPF-CNPJ-FORMATADO
                                       TO WDCM-CPF-CNPJ-AVAL-NOVO(IND-1)
                                          WDCM-CPF-CNPJ-AVAL-ASSINA2
           END-IF

           MOVE DCOMW90I-S-ENDERECO-AVAL-2NV
                                       TO WDCM-END-RES-AVAL-NOVO(IND-1)
           MOVE DCOMW90I-S-CIDADE-AVAL-2NV
                                       TO WDCM-NM-CIDADE-AVAL-NOVO
                                          (IND-1)
           MOVE DCOMW90I-S-ESTADO-AVAL-2NV
                                       TO WDCM-SIGLA-UF-AVAL-NOVO(IND-1)
           .

      *----------------------------------------------------------------*
       2370-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2380-MONTAR-ASSINATURA          SECTION.
      *----------------------------------------------------------------*

           MOVE DCOMW90I-S-NOME-TESTEMUNHA-1
                                       TO WDCM-NOME-TESTEMUNHA1

           MOVE DCOMW90I-S-CNPJ-TESTEMUNHA-1
                                       TO WRK-CNPJ-AUX-NUM
           MOVE DCOMW90I-S-FILIAL-TESTEMUNHA-1
                                       TO WRK-FILIAL-AUX
           MOVE DCOMW90I-S-CTRL-TESTEMUNHA-1
                                       TO WRK-CONTROLE-AUX

           PERFORM 5000-FORMATAR-CPF-CNPJ.

           MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF-CNPJ-TESTEMUNHA1

           MOVE DCOMW90I-S-NOME-TESTEMUNHA-2
                                       TO WDCM-NOME-AVAL-TESTEMUNHA2

           MOVE DCOMW90I-S-CNPJ-TESTEMUNHA-2
                                       TO WRK-CNPJ-AUX-NUM
           MOVE DCOMW90I-S-FIL-TESTEMUNHA-2
                                       TO WRK-FILIAL-AUX
           MOVE DCOMW90I-S-CTRL-TESTEMUNHA-2
                                       TO WRK-CONTROLE-AUX

           PERFORM 5000-FORMATAR-CPF-CNPJ.

           MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF-CNPJ-TESTEMINHA2

           MOVE DCOMW90I-S-NOME-CONJUGE-1NV
                                       TO WDCM-NOME-AVAL-ASS-CONJ1
           MOVE DCOMW90I-S-CNPJ-CONJUGE-1NV
                                       TO WRK-CNPJ-AUX-NUM
           MOVE DCOMW90I-S-FILIAL-CONJUGE-1NV
                                       TO WRK-FILIAL-AUX
           MOVE DCOMW90I-S-CTRL-CONJUGE-1NV
                                       TO WRK-CONTROLE-AUX

           PERFORM 5000-FORMATAR-CPF-CNPJ.

           MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF-CNPJ-AVAL-CONJ1

           MOVE DCOMW90I-S-NOME-CONJUGE-2NV
                                       TO WDCM-NOME-AVAL-ASS-CONJ2
           MOVE DCOMW90I-S-CNPJ-CONJUGE-2NV
                                       TO WRK-CNPJ-AUX-NUM
           MOVE DCOMW90I-S-FILIAL-CONJUGE-2NV
                                       TO WRK-FILIAL-AUX
           MOVE DCOMW90I-S-CTRL-CONJUGE-2NV
                                       TO WRK-CONTROLE-AUX

           PERFORM 5000-FORMATAR-CPF-CNPJ.

           MOVE WRK-CPF-CNPJ-FORMATADO TO WDCM-CPF-CNPJ-AVAL-CONJ2

           MOVE '"AGENCIA"'            TO WDCM-CAMPO-AGENCIA
           MOVE '"CONTA"'              TO WDCM-CAMPO-CONTA
           MOVE '"TAXA DE JUROS"'      TO WDCM-CAMPO-JUROS
           MOVE '"VALOR"'              TO WDCM-CAMPO-TITULO
           .
      *----------------------------------------------------------------*
       2380-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

      *--  AJUSTE PARA EVITAR S0C4 CASO ALGUM MODULO DE ERRO
      *--  ANTES DE ALOCAR A AREA DO DICM
           IF  WRK-ALOC-DICM NOT EQUAL 'S'
               PERFORM 8800-ALOCAR-DICM
           END-IF.

           MOVE LNK-AREA-DICM          TO DCOMWZ6I-S-AREA-DICM.
      *    MOV3 LNK-AREA-DICM          T  FEMPWZ4I-S-AREA-DICM.

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
4S2511     IF  WRK-FILIAL-AUX NOT EQUAL SPACES AND 
               WRK-FILIAL-AUX NOT EQUAL LOW-VALUES
               STRING WRK-CORPO-CPF-1 '.'
                      WRK-CORPO-CPF-2 '.'
                      WRK-CORPO-CPF-3 '/'
                      WRK-FILIAL-AUX  '-'
                      WRK-CONTROLE-AUX
               DELIMITED BY SIZE     INTO WRK-CPF-CNPJ-FORMATADO
           ELSE
               STRING WRK-CORPO-CPF-1 '.'
                      WRK-CORPO-CPF-2 '.'
                      WRK-CORPO-CPF-3 '-'
                      WRK-CONTROLE-AUX
               DELIMITED BY SIZE     INTO WRK-CPF-CNPJ-FORMATADO
           END-IF.

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
CD1912     INITIALIZE LNK-AREA-DICM.

 .    *----------------------------------------------------------------*
 .     8800-99-FIM.                    EXIT.
CD1912*----------------------------------------------------------------*
