      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT2460.
       AUTHOR MARCUS VINICIUS.
      *================================================================*
      *               W I P R O   -   I N F O S E R V E R              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT2460                                     *
      *    ANALISTA     : MARCUS VINICIUS - INFOSERVER - GRUPO 38      *
      *    DATA         : JAN/2017                                     *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERA ARQUIVO DE MIGRACAO PARA CXCE.                       *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                              INCLUDE/BOOK          *
      *      ARQENTRA                              GFCTWPCX            *
      *      ARQSAIDA                              GFCTWMCX            *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD7100 - MODULO TRATAMENTO DE ERRO.                     *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       INPUT-OUTPUT                    SECTION.
      *================================================================*
       FILE-CONTROL.

           SELECT ARQENTRA ASSIGN      TO UT-S-ARQENTRA
                      FILE STATUS      IS WRK-FS-ARQENTRA.

           SELECT ARQSAIDA ASSIGN      TO UT-S-ARQSAIDA
                      FILE STATUS      IS WRK-FS-ARQSAIDA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT:  ARQUIVO COM PREVIA PARA MIGRACAO CXCE                *
      *           ORG.SEQUENCIAL  -  LRECL = 97                        *
      *----------------------------------------------------------------*

       FD  ARQENTRA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTWPCX.

      *----------------------------------------------------------------*
      *  OUTPUT:  ARQUIVO PARA MIGRACAO CXCE                           *
      *           ORG.SEQUENCIAL  -  LRECL = 103                      *
      *----------------------------------------------------------------*

       FD  ARQSAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTWMCX.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*   INICIO DA WORKING GFCT2460 *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       01  ACU-ACUMULADORES.
           05 ACU-REG-LIDOS            PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-GRAVADOS         PIC 9(012) COMP-3   VALUE ZEROS.
           05 ACU-REG-DESPREZA         PIC 9(012) COMP-3   VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '* VARIAVEIS AUXILIARES         *'.
      *---------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-BATCH               PIC X(008)          VALUE
               'BATCH'.
           05  WRK-MASCARA             PIC ZZZ.ZZZ.ZZZ.ZZ9 VALUE ZEROS.

           05  WRK-XAV-COMPLETA.
             10  WRK-XAV-PART1.
               15  WRK-XAV-AGENCIA     PIC S9(005) COMP-3  VALUE ZEROES.
               15  WRK-XAV-CONTA       PIC S9(013) COMP-3  VALUE ZEROES.
               15  WRK-XAV-TARIFA      PIC S9(005) COMP-3  VALUE ZEROES.
             10  WRK-XAV-DTOCOR        PIC  X(010)         VALUE SPACES.

           05  WRK-ANT-COMPLETA.
             10  WRK-ANT-PART1.
               15  WRK-ANT-AGENCIA     PIC S9(005) COMP-3  VALUE ZEROES.
               15  WRK-ANT-CONTA       PIC S9(013) COMP-3  VALUE ZEROES.
               15  WRK-ANT-TARIFA      PIC S9(005) COMP-3  VALUE ZEROES.
             10  WRK-ANT-DTOCOR        PIC  X(010)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  AREA FILE-STATUS            *'.
      *----------------------------------------------------------------*

       01  WRK-FS-ARQENTRA             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-ARQSAIDA             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(032)          VALUE
           '*  MENSAGENS DE ERRO           *'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-FS.
           05  FILLER                  PIC X(007)          VALUE SPACES.
           05  FILLER                  PIC X(007)          VALUE
               '* ERRO '.
           05  WRK-OPERACAO            PIC X(013)          VALUE SPACES.
           05  FILLER                  PIC X(012)          VALUE
              ' DO ARQUIVO '.
           05  WRK-NOME-ARQ            PIC X(008)          VALUE SPACES.
           05  FILLER                  PIC X(017)          VALUE
              ' - FILE-STATUS = '.
           05  WRK-FS                  PIC X(002)          VALUE SPACES.
           05  FILLER                  PIC X(002)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*       AREA DA BRAD7100       *'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT2460   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0100-INICIALIZAR-PROGRAMA.

           PERFORM 0500-VERIFICA-VAZIO.

           PERFORM 0900-CONTROLE-PROCESSAMENTO
                    UNTIL     WRK-FS-ARQENTRA   EQUAL '10'.

           PERFORM 1800-PROCEDIMENTOS-FINAIS.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       0100-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ARQENTRA
                OUTPUT ARQSAIDA.

           MOVE  WRK-ABERTURA          TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *      TESTE DE FILE STATUS DOS ARQUIVOS                         *
      *----------------------------------------------------------------*
       0200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 0300-TESTAR-FS-ARQENTRA.

           PERFORM 0400-TESTAR-FS-ARQSAIDA.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ARQENTRA                    *
      *----------------------------------------------------------------*
       0300-TESTAR-FS-ARQENTRA         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQENTRA          NOT EQUAL '00'
              MOVE 'ARQENTRA'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ARQENTRA     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TESTE DE FILE STATUS DO ARQUIVO ARQSAIDA                    *
      *----------------------------------------------------------------*
       0400-TESTAR-FS-ARQSAIDA         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-ARQSAIDA          NOT EQUAL '00'
              MOVE 'ARQSAIDA'          TO WRK-NOME-ARQ
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE WRK-FS-ARQSAIDA     TO WRK-FS
              MOVE WRK-ERRO-FS         TO ERR-TEXTO

              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       0400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     VERIFICA SE OS ARQUIVOS DE ENTRADA ESTAO VAZIOS            *
      *----------------------------------------------------------------*
       0500-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 0600-LER-ARQUIVO-ARQENTRA.

           IF  WRK-FS-ARQENTRA         EQUAL '10'
               DISPLAY '***************** GFCT2460 *****************'
               DISPLAY '*                                          *'
               DISPLAY '*      ARQUIVO COM PREVIA PARA MIGRACAO    *'
               DISPLAY '*       CXCE VAZIO.                       *'
               DISPLAY '*                                          *'
               DISPLAY '***************** GFCT2460 *****************'
           END-IF.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     LEITURA DO ARQUIVO ARQENTRA                                *
      *----------------------------------------------------------------*
       0600-LER-ARQUIVO-ARQENTRA       SECTION.
      *----------------------------------------------------------------*

           READ ARQENTRA.

           IF  WRK-FS-ARQENTRA         EQUAL '10'
               GO                      TO 0600-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 0300-TESTAR-FS-ARQENTRA.

           ADD 1                       TO ACU-REG-LIDOS.

           MOVE  WPCX-CAG-DSTNO-MOVTO  TO WRK-XAV-AGENCIA.
           MOVE  WPCX-CCTA-DSTNO-MOVTO TO WRK-XAV-CONTA.
           MOVE  WPCX-CSERVC-TARIF     TO WRK-XAV-TARIFA.
           MOVE  WPCX-DOCOR-EVNTO      TO WRK-XAV-DTOCOR.

      *----------------------------------------------------------------*
       0600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO PRINCIPAL                                    *
      *----------------------------------------------------------------*
       0900-CONTROLE-PROCESSAMENTO     SECTION.
      *----------------------------------------------------------------*

           IF WRK-XAV-COMPLETA         EQUAL   WRK-ANT-COMPLETA
              PERFORM 1500-GRAVA-REGISTRO
              PERFORM 0600-LER-ARQUIVO-ARQENTRA
           ELSE
              IF WRK-XAV-PART1         EQUAL   WRK-ANT-PART1
                 ADD 1                 TO  ACU-REG-DESPREZA
                 PERFORM 0600-LER-ARQUIVO-ARQENTRA
              ELSE
                 MOVE WRK-XAV-COMPLETA TO WRK-ANT-COMPLETA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       0900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     GRAVA REGISTRO SAIDA                                       *
      *----------------------------------------------------------------*
       1500-GRAVA-REGISTRO             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE REG-GFCTWMCX.

           MOVE WPCX-CSERVC-TARIF        TO WMCX-CSERVC-TARIF.
           MOVE WPCX-CBCO-DSTNO-MOVTO    TO WMCX-CBCO-DSTNO-MOVTO.
           MOVE WPCX-CAG-DSTNO-MOVTO     TO WMCX-CAG-DSTNO-MOVTO.
           MOVE WPCX-CCTA-DSTNO-MOVTO    TO WMCX-CCTA-DSTNO-MOVTO.
           MOVE WPCX-DOCOR-EVNTO         TO WMCX-DOCOR-EVNTO.
           MOVE WPCX-VTARIF-CADTR-SIST   TO WMCX-VTARIF-CADTR-SIST.
           MOVE WPCX-VTARIF-BRUTO-MOVTO  TO WMCX-VTARIF-BRUTO-MOVTO.
           MOVE WPCX-VTARIF-LIQ-MOVTO    TO WMCX-VTARIF-LIQ-MOVTO.

           COMPUTE  WMCX-VTARIF-DSC-MOVTO  =
                WMCX-VTARIF-BRUTO-MOVTO  -  WMCX-VTARIF-LIQ-MOVTO.

           MOVE WPCX-VTARIF-DEB-MOVTO    TO WMCX-VTARIF-DEB-MOVTO.
           MOVE WPCX-DAGNDA-DEB-MOVTO    TO WMCX-DAGNDA-DEB-MOVTO.
           MOVE WPCX-DEFETV-DEB-MOVTO    TO WMCX-DEFETV-DEB-MOVTO.
           MOVE WPCX-QMAX-TENTV-DEB      TO WMCX-QMAX-TENTV-DEB.
           MOVE WPCX-QTENTV-DEB-MOVTO    TO WMCX-QTENTV-DEB-MOVTO.
           MOVE WPCX-CSIT-EVNTO-RECBD    TO WMCX-CSIT-EVNTO-RECBD.
           MOVE WPCX-CROTNA-ORIGE-MOVTO  TO WMCX-CROTNA-ORIGE-MOVTO.
           MOVE WPCX-DENVIO-MOVTO-TARIF  TO WMCX-DENVIO-MOVTO-TARIF.
           MOVE WPCX-CNRO-ARQ-MOVTO      TO WMCX-CNRO-ARQ-MOVTO.
           MOVE WPCX-CSEQ-MOVTO          TO WMCX-CSEQ-MOVTO.

           WRITE REG-GFCTWMCX.

           MOVE WRK-GRAVACAO           TO  WRK-OPERACAO.

           PERFORM 0400-TESTAR-FS-ARQSAIDA.

           ADD 1                       TO ACU-REG-GRAVADOS.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FINALIZACAO DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       1800-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           PERFORM 1900-EMITIR-DISPLAY-TOTAIS.

           CLOSE ARQENTRA
                 ARQSAIDA.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 0200-TESTAR-FILE-STATUS.

           STOP RUN.

      *----------------------------------------------------------------*
       1800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     EMITIR TOTAIS DE PROCESSAMENTO
      *----------------------------------------------------------------*
       1900-EMITIR-DISPLAY-TOTAIS      SECTION.
      *----------------------------------------------------------------*

           DISPLAY '                                            '.
           DISPLAY '***************** GFCT2460 *****************'.
           DISPLAY '*                                          *'.
           DISPLAY '*            PROGRAMA GFCT2460             *'.
           DISPLAY '*                                          *'.
           DISPLAY '*------------------------------------------*'.

           IF  ACU-REG-LIDOS           EQUAL ZEROS
               DISPLAY '*                                          *'
               DISPLAY '*         PROCESSAMENTO ENCERRADO          *'
           ELSE
               DISPLAY '*                                          *'
               DISPLAY '*   PROCESSAMENTO ENCERRADO COM SUCESSO    *'
               DISPLAY '*                                          *'
               DISPLAY '*------------------------------------------*'
               DISPLAY '*                                          *'

               MOVE ACU-REG-LIDOS          TO WRK-MASCARA

               DISPLAY '*  REGISTROS LIDOS       : ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-GRAVADOS       TO WRK-MASCARA

               DISPLAY '*  REGISTROS GRAVADOS    : ' WRK-MASCARA
                                                               ' *'

               MOVE ACU-REG-DESPREZA       TO WRK-MASCARA

               DISPLAY '*  REGISTROS DESPREZADOS : ' WRK-MASCARA
                                                               ' *'
           END-IF.

           DISPLAY '*                                          *'.
           DISPLAY '***************** GFCT2460 *****************'.

      *----------------------------------------------------------------*
       1900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNO DE ERRO                                            *
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT2460'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
