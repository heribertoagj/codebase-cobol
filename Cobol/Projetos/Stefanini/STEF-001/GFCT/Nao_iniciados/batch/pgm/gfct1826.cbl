      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. GFCT1826.
       AUTHOR.     SONDA PROCWORK.
      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT1826                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: UBIRAJARA (BIRA)                         *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: OUT/2010                                 *
      *                                                                *
      *    PROJETO..........: PWT-10-1993-EXTRATO ANUAL PARA ENVIO AO  *
      *                       PANAGON.                                 *
      *                                                                *
      *    OBJETIVO:                                                   *
      *             GERA ARQUIVO COM FORMATO DE EXTRATO ANUAL          *
      *             CONSOLIDADO, O ARQUIVO DE ENTRADA FOI GERADO       *
      *             PELO JOB DE EXPURGO DA BASE DB2 GFCTB0K5           *
      *             ATRAVEZ DO PGM GFCT1816.                           *
      *             OBS: ESTE PGM NAO TEM CONTROLE DE CARRO VISTO      *
      *                  QUE GERA RELATORIO E GRAVA EM ARQUIVO.        *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                                INCLUDE/BOOK        *
      *      BXEXTANO                               GFCTWADR           *
      *      EXTAVALI                               GFCTWAO1           *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      BRAD1050 - CONEXAO COM DB2.                               *
      *      BRAD7100 - MODULO TRATAMENTO DE ERRO.                     *
      *      GFCT5508 - OBTER NOME DO CORRENTISTA.                     *
      *      GFCT5527 - OBTER CPF/CNPJ/POSTO DE SERVICO.               *
      *                                                                *
BI1112*----------------------------------------------------------------*
BI1112*                 U L T I M A   A L T E R A C A O                *
BI1112*----------------------------------------------------------------*
BI1112*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI1012       *
BI1112*----------------------------------------------------------------*
BI1112*                                                                *
BI1112*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI1112*    DATA........:  10 / 2012                                    *
BI1112*    OBJETIVO....:  SUBSTITUIR A MENSAGEM (LEI 12.007)           *
BI1112*                   QUE SERA ACESSADA VIA MODULOS:               *
BI1112*                   EXGX1720 E EXGX1730.                         *
BI1112*                                                                *
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

           SELECT BXEXTANO ASSIGN      TO UT-S-BXEXTANO
                      FILE STATUS      IS WRK-FS-BXEXTANO.

           SELECT EXTAVALI ASSIGN      TO UT-S-EXTAVALI
                      FILE STATUS      IS WRK-FS-EXTAVALI.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *================================================================*
       FILE                            SECTION.
      *================================================================*

      *----------------------------------------------------------------*
      *   INPUT: HPU DA TABELA GFCTB0K5 (EXTRATO ANUAL)                *
      *           ORG.SEQUENCIAL  -  LRECL = 091                       *
      *----------------------------------------------------------------*

       FD  BXEXTANO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-BXEXTANO             PIC  X(091).

      *----------------------------------------------------------------*
      *  OUTPUT: ARQ.NO FORMATO DO LAYOUT DO EXTRATO PARA VALIDACAO    *
      *           ORG.SEQUENCIAL  -  LRECL = 080          (ANUAL)      *
      *----------------------------------------------------------------*

       FD  EXTAVALI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-EXTAVALI             PIC  X(080).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  INICIO DA WORKING GFCT1826  *'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *---------------------------------------------------------------*

       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-MODULO                  PIC  X(008)         VALUE ' '.
       01  WRK-RETURN-CODE             PIC S9(004)         VALUE ZEROS.
       01  WRK-CONT-LINHA              PIC S9(003) COMP-3  VALUE 0.
BI1112 01  WRK-QTD1-MSG                PIC S9(002) COMP-3  VALUE 0.
BI1112 01  WRK-QTD2-MSG                PIC S9(002) COMP-3  VALUE 0.
BI1112 01  WRK-IND                     PIC S9(003) COMP-3  VALUE 0.

       01  WRK-CONTA-R.
           05   FILLER                 PIC  X(006)         VALUE
                                                           '000000'.
           05   WRK-CTA7               PIC  9(007)         VALUE 0.
       01  WRK-CONTA-X   REDEFINES  WRK-CONTA-R.
           05   WRK-CONTA              PIC  9(013).

       01  FILLER.
           05   WRK-CTA13CS            PIC S9(013)         VALUE 0.
           05   WRK-CTA13X  REDEFINES  WRK-CTA13CS.
                10   WRK-CTA13SS       PIC  9(013).

       01  FILLER.
           05   WRK-AGE-5CS            PIC S9(005)         VALUE 0.
           05   WRK-AGE-5X  REDEFINES  WRK-AGE-5CS.
                10   WRK-AGE-5SS       PIC  9(005).

       01  FILLER.
           05   WRK-ANO-4CS            PIC S9(004)         VALUE 0.
           05   WRK-ANO-4X  REDEFINES  WRK-ANO-4CS.
                10   WRK-ANO-4SS       PIC  9(004).

       01  FILLER.
           05   WRK-TIPO-1CS            PIC S9(001)        VALUE 0.
           05   WRK-TIPO-1X  REDEFINES  WRK-TIPO-1CS.
                10   WRK-TIPO-1SS       PIC  9(001).

       01  FILLER.
           05   WRK-VALOR-11CS          PIC S9(009)V99     VALUE 0.
           05   WRK-VALOR-11X REDEFINES  WRK-VALOR-11CS.
                10   WRK-VALOR-11SS     PIC  9(009)V99.

       01  FILLER.
           05   WRK-QTDE-5CS            PIC S9(005)         VALUE 0.
           05   WRK-QTDE-5X  REDEFINES  WRK-QTDE-5CS.
                10   WRK-QTDE-5SS       PIC  9(005).

       01   WRK-CNPJ.
ST25X6*     05   WRK-CCGC              PIC  999.999.999  VALUE ZEROS.
ST25X6      05   WRK-CCGC              PIC  X(11)          VALUE SPACES.
            05   FILLER                PIC  X(001)         VALUE '/'.
ST25X6*     05   WRK-CFLIAL            PIC  9999           VALUE 0.
ST25X6      05   WRK-CFLIAL            PIC  X(4)           VALUE SPACES.
            05   FILLER                PIC  X(001)         VALUE '-'.
            05   WRK-CCTRL-CGC         PIC  99             VALUE 0.

       01   WRK-CHV-ATU.
            05   WRK-AGE-ATU           PIC  9(005)         VALUE 0.
            05   WRK-CTA-ATU           PIC  9(013)         VALUE 0.
            05   WRK-ANO-ATU           PIC  9(004)         VALUE 0.
            05   FILLER                PIC  X(002)    VALUE SPACES.
            05   WRK-TIPO-ATU          PIC  9(001)         VALUE 0.

       01   WRK-CHV-ANT.
            05   WRK-AGE-ANT           PIC  9(005)         VALUE 0.
            05   WRK-CTA-ANT           PIC  9(013)         VALUE 0.
            05   WRK-ANO-ANT           PIC  9(004)         VALUE 0.
            05   FILLER                PIC  X(002)    VALUE SPACES.
            05   WRK-TIPO-ANT          PIC  9(001)         VALUE 0.

       01   WRK-ANO-ATU-ALPHA          PIC  9(004) VALUE ZEROS.
       01   FILLER                     REDEFINES  WRK-ANO-ATU-ALPHA.
            05  WRK-ANO-ATU-ALPHA-R    PIC  X(004).

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  ACUMULADORES                *'.
      *---------------------------------------------------------------*

       01  WRK-TOTAIS.
           05   WRK-TOT-TIPO-TARIF     PIC S9(09)V99 COMP-3 VALUE 0.
           05   WRK-TOT-ANO            PIC S9(09)V99 COMP-3 VALUE 0.
           05   WRK-TOT-PEND           PIC S9(09)V99 COMP-3 VALUE 0.
           05   WRK-TOT-ESTOR          PIC S9(09)V99 COMP-3 VALUE 0.
           05   WRK-TOT-GERAL          PIC S9(09)V99 COMP-3 VALUE 0.

           05   WRK-TOT-CLI-ANO        PIC  9(11)   COMP-3  VALUE 0.
           05   WRK-TGRAVA-ANO         PIC  9(11)   COMP-3  VALUE 0.
           05   WRK-TOT-LIDO-ANU       PIC  9(11)   COMP-3  VALUE 0.

       01  WRK-EDT-TOTAIS              PIC  ZZ.ZZZ.ZZZ.ZZ9
                                       VALUE  ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  BOOK DO ARQUIVO BXEXTANO    *'.
      *---------------------------------------------------------------*

           COPY   GFCTWADR.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*  BOOK DO ARQUIVO EXTAVALI    *'.
      *---------------------------------------------------------------*

           COPY   GFCTWAO1.

BI1112*----------------------------------------------------------------*
BI1112 01  FILLER                      PIC  X(050)         VALUE
BI1112     '*   INCLUDE I#EXGXB1   - EXGX1720    *'.
BI1112*----------------------------------------------------------------*
BI1112
BI1112 01  WRK-EXGXB1-REGISTRO.
BI1112 COPY 'I#EXGXB1'.
BI1112
BI1112*----------------------------------------------------------------*
BI1112 01  FILLER                      PIC  X(050)         VALUE
BI1112     '*   INCLUDE I#EXGXB2   - EXGX1730    *'.
BI1112*----------------------------------------------------------------*
BI1112
BI1112 01  WRK-EXGXB2-REGISTRO.
BI1112 COPY 'I#EXGXB2'.

BI1112*----------------------------------------------------------------*
BI1112 01  FILLER                      PIC X(32)           VALUE
BI1112     '*  TABELA DE LINHAS LEI      *'.
BI1112*----------------------------------------------------------------*
BI1112 01  WRK-DADOS.
BI1112     05  FILLER               OCCURS 10.
BI1112         07  WRK-CPO-N        PIC  X(001).
BI1112         07  WRK-CPO-I        PIC  X(001).
BI1112         07  WRK-CPO-S        PIC  X(001).
BI1112         07  WRK-LINHA-LEI    PIC  X(080).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA TO FILE-STATUS       *'.
      *----------------------------------------------------------------*

       01  WRK-FS-BXEXTANO             PIC  X(002)         VALUE SPACES.
       01  WRK-FS-EXTAVALI             PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           'NA ABERTURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           'NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           'NA LEITURA   '.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*      MENSAGENS DE ERRO       *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FILE-STATUS.
           05  FILLER                  PIC  X(007)         VALUE SPACES.
           05  FILLER                  PIC  X(007)         VALUE
               '* ERRO '.
           05  WRK-MSG-FS-OPERACAO     PIC  X(013)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE
              ' DO ARQUIVO '.
           05  WRK-MSG-FS-NOME-ARQ     PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(017)         VALUE
              ' - FILE-STATUS = '.
           05  WRK-MSG-FS-CODIGO.
               10  WRK-MSG-FS-CODIGO-N PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  MENSAGEM DE ACESSO A MODULO *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
BI1112     03  WRK-MSG01.
BI1112         05  FILLER              PIC X(54)           VALUE
BI1112         'MENSAGEM (LEI 12.007) NAO CADASTRADA NA BASE EXGXB055 '.
BI1112         05  FILLER              PIC X(11)           VALUE
BI1112         'PARA O ANO '.
BI1112         05  WRK-MSG1-ANO        PIC 9(04)           VALUE ZEROS.
BI1112         05  FILLER              PIC X(01)           VALUE SPACES.

BI1112     03  WRK-MSG02.
BI1112         05  FILLER              PIC X(54)           VALUE
BI1112         'MENSAGEM (LEI 12.007) NAO CADASTRADA NA BASE EXGXB045 '.
BI1112         05  FILLER              PIC X(11)           VALUE
BI1112         'PARA O ANO '.
BI1112         05  WRK-MSG2-ANO        PIC 9(04)           VALUE ZEROS.
BI1112         05  FILLER              PIC X(01)           VALUE SPACES.

           03  WRK-MSG03.
               05  FILLER              PIC X(26)           VALUE
                   'ERRO NA CHAMADA AO MODULO '.
               05  WRK-MODULO-MSG      PIC X(08)           VALUE SPACES.
               05  FILLER              PIC X(41)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*        AREA BRAD7100         *'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(019)         VALUE
           ' AREA PARA ERROS'.
      *----------------------------------------------------------------*

       COPY 'I#GFCT0M'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA DE MODULO GFCT5508    *'.
      *----------------------------------------------------------------*

       01  WRK-5508-AREA-ENTRADA.
           03  WRK-5508-AMBIENTE       PIC X(01)           VALUE SPACES.
           03  WRK-5508-CJUNC-DEPDC    PIC 9(05)           VALUE ZEROS.
           03  WRK-5508-CCTA-CLI       PIC 9(07)           VALUE ZEROS.

       01  WRK-5508-AREA-SAIDA.
           03  WRK-5508-COD-RETORNO    PIC 9(02)           VALUE ZEROS.
           03  WRK-5508-COD-SQL-ERRO   PIC 9(03)           VALUE ZEROS.
           03  WRK-5508-DESC-MSG       PIC X(70)           VALUE SPACES.
           03  WRK-5508-DADOS-RETORNO.
               05  WRK-5508-IPSSOA-COPLT
                                       PIC X(70)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA DE COMUNICACAO COM GFCT5527 *'.
      *----------------------------------------------------------------*

       01  WRK-5527-AREA-ENTRADA.
           05  WRK-5527-AMBIENTE           PIC  X(001) VALUE SPACES.
           05  WRK-5527-CHAMADOR           PIC  X(008) VALUE SPACES.
           05  WRK-5527-AGENCIA-CLIENTE    PIC  9(005) VALUE ZEROS.
           05  WRK-5527-CONTA-CLIENTE      PIC  9(013) VALUE ZEROS.

       01  WRK-5527-AREA-SAIDA.
            05  WRK-5527-COD-RETORNO        PIC  9(002) VALUE ZEROS.
            05  WRK-5527-COD-SQL-ERRO       PIC S9(003) VALUE ZEROS.
            05  WRK-5527-COD-MSG-GFCT       PIC  9(004) VALUE ZEROS.
            05  WRK-5527-DESC-MSG           PIC  X(070) VALUE SPACES.
            05  WRK-5527-QTD-OCORR          PIC  9(005) VALUE ZEROS.
            05  WRK-5527-DADOS-RETORNO.
ST25X6*     10  WRK-5527-CCGC-CPF         PIC  9(009) VALUE ZEROS.
ST25X6      10  WRK-5527-CCGC-CPF         PIC  X(009) VALUE SPACES.
ST25X6*     10  WRK-5527-CFLIAL-CGC       PIC  9(004) VALUE ZEROS.
ST25X6      10  WRK-5527-CFLIAL-CGC       PIC  X(004) VALUE SPACES.
            10  WRK-5527-CCTRL-CPF-CGC    PIC  9(002) VALUE ZEROS.
            10  WRK-5527-CPOSTO-SERVC     PIC  9(003) VALUE ZEROS.
            10  WRK-5527-CSGMTO-CLI       PIC  9(003) VALUE ZEROS.

       01  WRK-IO-PCB.
           03  WRK-IO-TERM              PIC  X(008) VALUE SPACES.
           03  FILLER                   PIC  X(002) VALUE SPACES.
           03  WRK-IO-STA               PIC  X(002) VALUE SPACES.
           03  FILLER                   PIC  X(012) VALUE SPACES.
           03  WRK-IO-MODNAME           PIC  X(008) VALUE SPACES.

       01  WRK-ALT-PCB.
           03  WRK-ALT-PCBNAME          PIC  X(008) VALUE SPACES.
           03  FILLER                   PIC  X(002) VALUE SPACES.
           03  WRK-ALT-STATUS           PIC  X(002) VALUE SPACES.
           03  FILLER                   PIC  X(012) VALUE SPACES.
           03  WRK-ALT-MODNAME          PIC  X(008) VALUE SPACES.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL                                            *
      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 8900-INICIALIZAR-PROGRAMA.

           PERFORM 2000-PROCESSA-ANO.

           PERFORM 9000-PROCEDIMENTOS-FINAIS.

           STOP RUN.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSA-ANO                 SECTION.
      *----------------------------------------------------------------*

      *----->  PRIMEIRA LEITURA DO ARQUIVO

           PERFORM   7300-LER-BXEXTANO.

           MOVE   WRK-AGE-ATU   TO   WRK-AGE-ANT.
           MOVE   WRK-CTA-ATU   TO   WRK-CTA-ANT.
           MOVE   WRK-ANO-ATU   TO   WRK-ANO-ANT.
           MOVE   WRK-TIPO-ATU  TO   WRK-TIPO-ANT.

           PERFORM   2100-PROCESSA-AGE-CTA-ANUAL
                     UNTIL   WRK-FS-BXEXTANO EQUAL  '10'.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-PROCESSA-AGE-CTA-ANUAL       SECTION.
      *----------------------------------------------------------------*

           MOVE   ZEROS   TO   WRK-TOT-PEND   WRK-TOT-ANO
                               WRK-TOT-ESTOR  WRK-TOT-GERAL

           ADD     1      TO   WRK-TOT-CLI-ANO.

           PERFORM   5000-CABECALHO-AGE-CONTA

           PERFORM   5150-CABECALHO-ANO

           PERFORM   2200-EMITE-TIPO-ANUAL
             UNTIL   WRK-AGE-ANT  NOT EQUAL WRK-AGE-ATU  OR
                     WRK-CTA-ANT  NOT EQUAL WRK-CTA-ATU  OR
                     WRK-ANO-ANT  NOT EQUAL WRK-ANO-ATU

           PERFORM   5400-TOTAL-DO-ANO

           MOVE   WRK-AGE-ATU   TO   WRK-AGE-ANT.
           MOVE   WRK-CTA-ATU   TO   WRK-CTA-ANT.
           MOVE   WRK-ANO-ATU   TO   WRK-ANO-ANT.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-EMITE-TIPO-ANUAL             SECTION.
      *----------------------------------------------------------------*

           MOVE   ZEROS   TO   WRK-TOT-TIPO-TARIF.

           IF   WRK-TIPO-ATU   NOT EQUAL  5  AND 4
                PERFORM   5200-CABECALHO-TIPO-SERV
           END-IF

           PERFORM   2300-EMITE-TARIFA-ANUAL
             UNTIL   WRK-AGE-ANT   NOT EQUAL WRK-AGE-ATU  OR
                     WRK-CTA-ANT   NOT EQUAL WRK-CTA-ATU  OR
                     WRK-ANO-ANT   NOT EQUAL WRK-ANO-ATU  OR
                     WRK-TIPO-ANT  NOT EQUAL WRK-TIPO-ATU

           IF  WRK-TIPO-ANT     EQUAL   5
               NEXT  SENTENCE
           ELSE
               IF  WRK-TIPO-ANT   EQUAL  4
                   ADD    WRK-TOT-TIPO-TARIF  TO   WRK-TOT-ESTOR
               ELSE
                   PERFORM  5600-TOTAL-TIPO
                   ADD    WRK-TOT-TIPO-TARIF  TO   WRK-TOT-ANO
               END-IF
           END-IF.

           MOVE   WRK-TIPO-ATU   TO   WRK-TIPO-ANT.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-EMITE-TARIFA-ANUAL           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TIPO-ATU   EQUAL 5
               ADD  WADR-VCONSL-DETRM        TO  WRK-TOT-PEND
           ELSE
               PERFORM  2400-DETALHE-ANUAL
           END-IF.

           PERFORM   7300-LER-BXEXTANO.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-DETALHE-ANUAL              SECTION.
      *----------------------------------------------------------------*

           MOVE  WADR-RSEGDA-LIN         TO   WRK-L04-DESCR-SERV

           IF  WADR-CIDTFD-TARIF EQUAL 1 OR 2  OR   3
               MOVE  WADR-QCONSL-DETRM        TO  WRK-QTDE-5CS
               MOVE  WRK-QTDE-5SS             TO  WRK-L04-QTDE
           ELSE
               MOVE  SPACES                   TO  WRK-L04-QTDE-X
           END-IF.

           MOVE  WADR-VCONSL-DETRM        TO  WRK-VALOR-11CS
           MOVE  WRK-VALOR-11SS           TO  WRK-L04-VALOR
           ADD   WADR-VCONSL-DETRM        TO  WRK-TOT-TIPO-TARIF.

           IF WRK-TIPO-ATU           NOT EQUAL  4
              MOVE   WRK-LINHA04     TO   WRK-LINHA
              PERFORM  5900-GRAVAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5000-CABECALHO-AGE-CONTA        SECTION.
      *----------------------------------------------------------------*

           IF    WRK-MODULO  NOT EQUAL  SPACES
                 PERFORM  6000-MSG-LEI
           END-IF.

           MOVE  WRK-AGE-ATU   TO  WRK-L01-AGENCIA
           MOVE  WRK-CTA-ATU   TO  WRK-CONTA
           MOVE  WRK-CTA7      TO  WRK-L01-CONTA
           MOVE  WRK-ANO-ATU   TO  WRK-ANO-ATU-ALPHA.
           MOVE  WRK-ANO-ATU-ALPHA-R   TO  WRK-L01B-ANO.

      *--> ACESSAR NOME DO CLIENTE E CNPJ/CPF

           PERFORM 5010-SELECIONAR-NOME.

           PERFORM 5020-OBTER-CNPJ-CPF-POSTO

           MOVE     ZEROS            TO WRK-CONT-LINHA.

           MOVE     WRK-LINHA01      TO WRK-LINHA.
           PERFORM  5900-GRAVAR.

           MOVE     WRK-LINHA-SP1    TO WRK-LINHA.
           PERFORM  5900-GRAVAR.

           MOVE     WRK-LINHA01B     TO WRK-LINHA.
           PERFORM  5900-GRAVAR.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5010-SELECIONAR-NOME                    SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5508-AREA-SAIDA.

           MOVE 'B'                    TO WRK-5508-AMBIENTE.
           MOVE WRK-AGE-ATU            TO WRK-5508-CJUNC-DEPDC.
           MOVE WRK-CTA-ATU(7:7)       TO WRK-5508-CCTA-CLI.
           MOVE 'GFCT5508'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5508-AREA-ENTRADA
                                             WRK-5508-AREA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'GFCT5508'         TO WRK-MODULO-MSG
               MOVE WRK-MSG03          TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  WRK-5508-COD-RETORNO    EQUAL 01 OR 99
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-5508-DESC-MSG  TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  WRK-5508-COD-RETORNO    EQUAL 02
               MOVE SPACES             TO WRK-L01-NOME-CLI
           ELSE
               MOVE WRK-5508-IPSSOA-COPLT(1:40)
                                       TO WRK-L01-NOME-CLI
           END-IF.

      *----------------------------------------------------------------*
       5010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5020-OBTER-CNPJ-CPF-POSTO               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES         TO WRK-5527-AREA-SAIDA.

           INITIALIZE WRK-5527-AREA-SAIDA
                      GFCT0M-AREA-ERROS
                      WRK-IO-PCB
                      WRK-ALT-PCB.

           MOVE 'GFCT1570'     TO WRK-5527-CHAMADOR.
           MOVE WRK-AGE-ATU    TO WRK-5527-AGENCIA-CLIENTE.
           MOVE WRK-CTA-ATU    TO WRK-5527-CONTA-CLIENTE.
           MOVE 'B'            TO WRK-5527-AMBIENTE.

           MOVE 'GFCT5527'     TO WRK-MODULO.

           CALL WRK-MODULO        USING WRK-5527-AREA-ENTRADA
                                        WRK-5527-AREA-SAIDA
                                        GFCT0M-AREA-ERROS
                                        WRK-IO-PCB
                                        WRK-ALT-PCB.

           IF  RETURN-CODE     NOT EQUAL  ZEROS
ST25X6      MOVE SPACES     TO WRK-5527-CCGC-CPF
ST25X6                          WRK-5527-CFLIAL-CGC
            MOVE ZEROS      TO WRK-5527-CCTRL-CPF-CGC
           ELSE
            IF  WRK-5527-COD-RETORNO    NOT EQUAL  ZEROS
ST25X6          MOVE SPACES              TO WRK-5527-CCGC-CPF
ST25X6                                      WRK-5527-CFLIAL-CGC
                MOVE ZEROS              TO WRK-5527-CCTRL-CPF-CGC
               END-IF
           END-IF.

ST25X6      IF   WRK-5527-CFLIAL-CGC  NOT EQUAL  SPACES
            MOVE  WRK-5527-CCGC-CPF       TO  WRK-CCGC
            MOVE  WRK-5527-CFLIAL-CGC     TO  WRK-CFLIAL
            MOVE  WRK-5527-CCTRL-CPF-CGC  TO  WRK-CCTRL-CGC
                MOVE  WRK-CNPJ                TO  WRK-L01B-CNPJ-CPF
           ELSE
ST25X6      IF  WRK-5527-CCGC-CPF  NOT EQUAL  SPACES
            MOVE  WRK-5527-CCGC-CPF       TO  WRK-CCGC
ST25X6      MOVE  SPACES                  TO  WRK-CFLIAL
            MOVE  WRK-5527-CCTRL-CPF-CGC  TO  WRK-CCTRL-CGC
                    MOVE  WRK-CNPJ                TO  WRK-L01B-CNPJ-CPF
                ELSE
                    MOVE  SPACES                  TO  WRK-L01B-CNPJ-CPF
                END-IF
           END-IF.

      *----------------------------------------------------------------*
       5020-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5150-CABECALHO-ANO              SECTION.
      *----------------------------------------------------------------*

           MOVE   WRK-LINHA-SP                TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

           MOVE     WRK-LINHA00  TO WRK-LINHA
           PERFORM  5900-GRAVAR.

           MOVE   WRK-LINHA-SP                TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

           MOVE   WRK-ANO-ATU                 TO   WRK-L02A-ANO-LANC
           MOVE   WRK-LINHA02A                TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

      *----------------------------------------------------------------*
       5150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5200-CABECALHO-TIPO-SERV        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TIPO-ATU  EQUAL 1
               MOVE   'CESTA DE SERVICOS'    TO  WRK-L03-LIT-SERV
               MOVE   'QTDE PAGA'            TO  WRK-L03-LIT-QTDE
               MOVE   'VALOR PAGO'           TO  WRK-L03-LIT-VALOR
           ELSE
             IF  WRK-TIPO-ATU EQUAL 2
                 MOVE   'SERVICOS ESSENCIAIS EXCEDENTES A FRANQUIAS'
                                             TO  WRK-L03-LIT-SERV
                 MOVE   'QTDE PAGA'          TO  WRK-L03-LIT-QTDE
                 MOVE   'VALOR PAGO'         TO  WRK-L03-LIT-VALOR
             ELSE
               IF  WRK-TIPO-ATU EQUAL 3
                   MOVE   'SERVICOS AVULSOS' TO  WRK-L03-LIT-SERV
                   MOVE   'QTDE PAGA'        TO  WRK-L03-LIT-QTDE
                   MOVE   'VALOR PAGO'       TO  WRK-L03-LIT-VALOR
               END-IF
             END-IF
           END-IF.

           MOVE   WRK-LINHA-SP                TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

           MOVE     WRK-LINHA03              TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5400-TOTAL-DO-ANO               SECTION.
      *----------------------------------------------------------------*

           MOVE   WRK-LINHA-SP        TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

           MOVE   WRK-TOT-ANO         TO   WRK-VALOR-11CS
           MOVE   WRK-VALOR-11SS      TO   WRK-L07-TOT-ANO
           MOVE   WRK-LINHA07         TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

           IF  WRK-TOT-ESTOR   NOT EQUAL ZEROS
               PERFORM  5700-TOTAL-ESTORNADO
           END-IF.

           PERFORM 5800-TOTAL-GERAL.

BI1112     IF   WRK-L02A-ANO-LANC   EQUAL   EXGXB2-ANO-EXTRT
BI1112          GO                  TO      5400-99-FIM
BI1112     END-IF.
BI1112
BI1112     PERFORM 5810-ACESSAR-LEI-PARTE-I.
BI1112
BI1112     PERFORM 5820-ACESSAR-LEI-PARTE-II.

      *----------------------------------------------------------------*
       5400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5600-TOTAL-TIPO                 SECTION.
      *----------------------------------------------------------------*

           MOVE   WRK-LINHA00         TO   WRK-LINHA.
           MOVE   SPACES              TO   WRK-LINHA(3:54)
           MOVE   SPACES              TO   WRK-LINHA(71:10)
           PERFORM  5900-GRAVAR.

           MOVE   WRK-TOT-TIPO-TARIF  TO   WRK-VALOR-11CS
           MOVE   WRK-VALOR-11SS      TO   WRK-L05-TOT-TIPO
           MOVE   WRK-LINHA05         TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

      *----------------------------------------------------------------*
       5600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5700-TOTAL-ESTORNADO            SECTION.
      *----------------------------------------------------------------*

           MOVE   WRK-LINHA-SP        TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

           MOVE   WRK-TOT-ESTOR       TO   WRK-VALOR-11CS
           MOVE   WRK-VALOR-11SS      TO   WRK-L09-TOT-ESTOR
           MOVE   WRK-LINHA09         TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

      *----------------------------------------------------------------*
       5700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5800-TOTAL-GERAL                SECTION.
      *----------------------------------------------------------------*

           IF   WRK-TOT-ESTOR   LESS WRK-TOT-ANO
                COMPUTE WRK-TOT-GERAL = WRK-TOT-ANO - WRK-TOT-ESTOR
           ELSE
                MOVE ZEROS     TO WRK-TOT-GERAL
           END-IF.

           MOVE   WRK-LINHA-SP        TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

           MOVE   WRK-TOT-GERAL       TO   WRK-VALOR-11CS
           MOVE   WRK-VALOR-11SS      TO   WRK-L10-TOT-GER
           MOVE   WRK-LINHA10         TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

      *----------------------------------------------------------------*
       5800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

BI1112*----------------------------------------------------------------*
       5810-ACESSAR-LEI-PARTE-I        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE EXGXB2-REG.

           MOVE 'GFCT1826'             TO EXGXB2-TRANSACAO.
           MOVE 1                      TO EXGXB2-PESQUISA
                                          EXGXB2-VERSAO.
           MOVE 2                      TO EXGXB2-ORIGEM.
           MOVE 80                     TO EXGXB2-TAM-MIDIA.
           MOVE WRK-L02A-ANO-LANC      TO EXGXB2-ANO-EXTRT.

           MOVE 'EXGX1730'             TO WRK-MODULO.

           CALL WRK-MODULO          USING  EXGXB2-REG.

           IF  RETURN-CODE             NOT  EQUAL   ZEROS
               DISPLAY '*-------------------------------------------*'
               DISPLAY '*   ROTINA DE EXPURGO DA BASE GFCTB0K5      *'
               DISPLAY '*           PROGRAMA  GFCT1826              *'
               DISPLAY '*           ------------------              *'
               DISPLAY '*                                           *'
               DISPLAY '*   ERRO NA CHAMADA DO MODULO EXGX1730 ,    *'
               DISPLAY '* MENSAGENS (LEI 12.007) NAO CADASTRADAS NA *'
               DISPLAY '* BASE EXGXB055 PARA O ANO ' WRK-L02A-ANO-LANC
               DISPLAY '*                                           *'
               DISPLAY '* RC             => ' RETURN-CODE
               DISPLAY '* EXGXB2-ENTRADA => ' EXGXB2-ENTRADA
               DISPLAY '*-------------------------------------------*'
               MOVE 'APL'                   TO ERR-TIPO-ACESSO
               MOVE 'EXGX1730'              TO WRK-MODULO-MSG
               MOVE WRK-L02A-ANO-LANC       TO WRK-MSG1-ANO
               MOVE WRK-MSG01               TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  EXGXB2-COD-RETORNO      NOT  EQUAL   ZEROS
               DISPLAY '*-------------------------------------------*'
               DISPLAY '*   ROTINA DE EXPURGO DA BASE GFCTB0K5      *'
               DISPLAY '*           PROGRAMA  GFCT1826              *'
               DISPLAY '*           ------------------              *'
               DISPLAY '*                                           *'
               DISPLAY '*   ERRO NA CHAMADA DO MODULO EXGX1730 ,    *'
               DISPLAY '* MENSAGENS (LEI 12.007) NAO CADASTRADAS NA *'
               DISPLAY '* BASE EXGXB055 PARA O ANO ' WRK-L02A-ANO-LANC
               DISPLAY '*                                           *'
               DISPLAY '* COD. RETORNO   => ' EXGXB2-COD-RETORNO
               DISPLAY '* EXGXB2-ENTRADA => ' EXGXB2-ENTRADA
               DISPLAY '*-------------------------------------------*'
               MOVE 'APL'                   TO ERR-TIPO-ACESSO
               MOVE 'EXGX1730'              TO WRK-MODULO-MSG
               MOVE WRK-L02A-ANO-LANC       TO WRK-MSG1-ANO
               MOVE WRK-MSG01               TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       5810-99-FIM.                    EXIT.
BI1112*----------------------------------------------------------------*

BI1112*----------------------------------------------------------------*
       5820-ACESSAR-LEI-PARTE-II       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE EXGXB1-REG.

           MOVE 'GFCT1826'             TO EXGXB1-TRANSACAO.
           MOVE 1                      TO EXGXB1-PESQUISA
                                          EXGXB1-VERSAO.
           MOVE 2                      TO EXGXB1-ORIGEM.
           MOVE 1                      TO EXGXB1-MIDIA.
           MOVE ZEROS                  TO EXGXB1-TPO-MIDIA.
           MOVE 80                     TO EXGXB1-TAM-MIDIA.
           MOVE ZEROS                  TO EXGXB1-ANO-EXTRT.

           MOVE 'EXGX1720'             TO WRK-MODULO.

           CALL WRK-MODULO          USING EXGXB1-REG.

           IF  RETURN-CODE             NOT  EQUAL ZEROS
               DISPLAY '*-------------------------------------------*'
               DISPLAY '*   ROTINA DE EXPURGO DA BASE GFCTB0K5      *'
               DISPLAY '*           PROGRAMA  GFCT1826              *'
               DISPLAY '*           ------------------              *'
               DISPLAY '*                                           *'
               DISPLAY '*   ERRO NA CHAMADA DO MODULO EXGX1720 ,    *'
               DISPLAY '* MENSAGENS (LEI 12.007) NAO CADASTRADAS NA *'
               DISPLAY '* BASE EXGXB045 PARA O ANO ' WRK-L02A-ANO-LANC
               DISPLAY '*                                           *'
               DISPLAY '* RC             => ' RETURN-CODE
               DISPLAY '* EXGXB1-ENTRADA => ' EXGXB1-ENTRADA
               DISPLAY '*-------------------------------------------*'
               MOVE 'APL'                   TO ERR-TIPO-ACESSO
               MOVE 'EXGX1720'              TO WRK-MODULO-MSG
               MOVE WRK-L02A-ANO-LANC       TO WRK-MSG2-ANO
               MOVE WRK-MSG02               TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  EXGXB1-COD-RETORNO      NOT  EQUAL   ZEROS
               DISPLAY '*-------------------------------------------*'
               DISPLAY '*   ROTINA DE EXPURGO DA BASE GFCTB0K5      *'
               DISPLAY '*           PROGRAMA  GFCT1826              *'
               DISPLAY '*           ------------------              *'
               DISPLAY '*                                           *'
               DISPLAY '*   ERRO NA CHAMADA DO MODULO EXGX1720 ,    *'
               DISPLAY '* MENSAGENS (LEI 12.007) NAO CADASTRADAS NA *'
               DISPLAY '* BASE EXGXB045 PARA O ANO ' WRK-L02A-ANO-LANC
               DISPLAY '*                                           *'
               DISPLAY '* COD. RETORNO   => ' EXGXB1-COD-RETORNO
               DISPLAY '* EXGXB1-ENTRADA => ' EXGXB1-ENTRADA
               DISPLAY '*-------------------------------------------*'
               MOVE 'APL'                   TO ERR-TIPO-ACESSO
               MOVE 'EXGX1720'              TO WRK-MODULO-MSG
               MOVE WRK-L02A-ANO-LANC       TO WRK-MSG2-ANO
               MOVE WRK-MSG02               TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       5820-99-FIM.                    EXIT.
BI1112*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5900-GRAVAR                     SECTION.
      *----------------------------------------------------------------*

           MOVE   WRK-GRAVACAO        TO   WRK-MSG-FS-OPERACAO.

           WRITE  FD-REG-EXTAVALI     FROM WRK-LINHA
           PERFORM   9235-TESTAR-FS-EXTAVALI
           ADD     1                  TO   WRK-TGRAVA-ANO.

           ADD     1                  TO   WRK-CONT-LINHA.

           IF    (WRK-CONT-LINHA  GREATER  63)
                  PERFORM 6100-QUEBRA-PAGINA
                  MOVE 3              TO  WRK-CONT-LINHA
           END-IF.

      *----------------------------------------------------------------*
       5900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6000-MSG-LEI                    SECTION.
      *----------------------------------------------------------------*

           MOVE     WRK-LINHA-SP      TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.
           MOVE     WRK-LINHA-SP      TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

BI1112     MOVE EXGXB2-OCC-MSG  TO WRK-DADOS(1:830).
BI1112     MOVE EXGXB2-QTD-MSG  TO WRK-QTD2-MSG.
BI1112     MOVE ZEROS           TO WRK-IND.

BI1112     PERFORM
BI1112     VARYING WRK-QTD2-MSG        FROM WRK-QTD2-MSG BY -1
BI1112       UNTIL WRK-QTD2-MSG        LESS 1
BI1112          ADD      1                      TO   WRK-IND
BI1112          MOVE     '2 '                   TO   WRK-LINHA(1:2)
BI1112          MOVE     WRK-LINHA-LEI(WRK-IND)(1:78)
BI1112                                          TO   WRK-LINHA(3:78)
BI1112          PERFORM  5900-GRAVAR
BI1112     END-PERFORM.

BI1112     MOVE     WRK-LINHA-SP      TO   WRK-LINHA.
BI1112     PERFORM  5900-GRAVAR.

BI1112     MOVE EXGXB1-OCC-MSG  TO WRK-DADOS(1:830).
BI1112     MOVE EXGXB1-QTD-MSG  TO WRK-QTD1-MSG.
BI1112     MOVE ZEROS           TO WRK-IND.

BI1112     PERFORM
BI1112     VARYING WRK-QTD1-MSG        FROM WRK-QTD1-MSG BY -1
BI1112       UNTIL WRK-QTD1-MSG        LESS 1
BI1112          ADD      1                      TO   WRK-IND
BI1112          MOVE     '2 '                   TO   WRK-LINHA(1:2)
BI1112          MOVE     WRK-LINHA-LEI(WRK-IND)(1:78)
BI1112                                          TO   WRK-LINHA(3:78)
BI1112          PERFORM  5900-GRAVAR
BI1112     END-PERFORM.


BI1110     MOVE   WRK-LINHA-SP        TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.
           MOVE   WRK-LINHA-SP        TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

      *----------------------------------------------------------------*
       6000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       6100-QUEBRA-PAGINA              SECTION.
      *----------------------------------------------------------------*

           MOVE     WRK-LINHA01      TO WRK-LINHA.
           WRITE    FD-REG-EXTAVALI  FROM WRK-LINHA.
           PERFORM  9235-TESTAR-FS-EXTAVALI.

           MOVE     WRK-LINHA-SP1    TO WRK-LINHA.
           WRITE    FD-REG-EXTAVALI  FROM WRK-LINHA.
           PERFORM  9235-TESTAR-FS-EXTAVALI.

           MOVE     WRK-LINHA01B     TO WRK-LINHA.
           WRITE    FD-REG-EXTAVALI  FROM WRK-LINHA.
           PERFORM  9235-TESTAR-FS-EXTAVALI.

      *----------------------------------------------------------------*
       6100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7300-LER-BXEXTANO               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-LEITURA            TO WRK-MSG-FS-OPERACAO.

           READ  BXEXTANO              INTO  GFCTWADR-REG

           PERFORM 9220-TESTAR-FS-BXEXTANO

           IF WRK-FS-BXEXTANO  EQUAL  '10'
              MOVE 99999               TO  WRK-AGE-ATU
              MOVE 9999999999999       TO  WRK-CTA-ATU
              MOVE 9999                TO  WRK-ANO-ATU
              MOVE  9                  TO  WRK-TIPO-ATU
           ELSE
              MOVE WADR-CAG-BCRIA      TO  WRK-AGE-5CS
              MOVE WRK-AGE-5SS         TO  WRK-AGE-ATU
              MOVE WADR-CCTA-BCRIA-CLI TO  WRK-CTA13CS
              MOVE WRK-CTA13SS         TO  WRK-CTA-ATU
              MOVE WADR-DANO-REFT      TO  WRK-ANO-4CS
              MOVE WRK-ANO-4SS         TO  WRK-ANO-ATU
              MOVE WADR-CIDTFD-TARIF   TO  WRK-TIPO-1CS
              MOVE WRK-TIPO-1SS        TO  WRK-TIPO-ATU
              ADD    1                 TO  WRK-TOT-LIDO-ANU
           END-IF.

      *----------------------------------------------------------------*
       7300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       8900-INICIALIZAR-PROGRAMA       SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT   BXEXTANO
               OUTPUT   EXTAVALI.

           MOVE WRK-ABERTURA           TO WRK-MSG-FS-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       8900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-PROCEDIMENTOS-FINAIS       SECTION.
      *----------------------------------------------------------------*

           MOVE   WRK-LINHA-SP        TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.
           MOVE   WRK-LINHA-SP        TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.
           MOVE   WRK-LINHA-SP        TO   WRK-LINHA.
           PERFORM  5900-GRAVAR.

BI1112     PERFORM  6000-MSG-LEI.

           PERFORM 9100-EMITIR-DISPLAY.

           CLOSE BXEXTANO
                 EXTAVALI.

           MOVE WRK-FECHAMENTO         TO WRK-MSG-FS-OPERACAO.

           PERFORM 9200-TESTAR-FILE-STATUS.

           MOVE WRK-RETURN-CODE        TO RETURN-CODE.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*                                               *'.
           DISPLAY '*-----------------------------------------------*'.
           DISPLAY '*                                               *'.
           DISPLAY '*           PROGRAMA  GFCT1826                  *'.
           DISPLAY '*           ------------------                  *'.
           DISPLAY '*                                               *'.
           DISPLAY '*     PROCESSAMENTO EFETUADO COM SUCESSO        *'.
           DISPLAY '*                                               *'.
           DISPLAY '*-----------------------------------------------*'.
           MOVE     WRK-TOT-LIDO-ANU  TO  WRK-EDT-TOTAIS
           DISPLAY '* TOTAL DE LIDOS BXEXTANO = ' WRK-EDT-TOTAIS
           MOVE     WRK-TGRAVA-ANO     TO  WRK-EDT-TOTAIS
           DISPLAY '* TOTAL DE GRAVA EXTAVALI = ' WRK-EDT-TOTAIS
           MOVE     WRK-TOT-CLI-ANO   TO  WRK-EDT-TOTAIS
           DISPLAY '* TOT. EXTRATO CLIENTE ANO= ' WRK-EDT-TOTAIS
           DISPLAY '*-----------------------------------------------*'.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 9220-TESTAR-FS-BXEXTANO.

           PERFORM 9235-TESTAR-FS-EXTAVALI.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9220-TESTAR-FS-BXEXTANO         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-BXEXTANO       NOT EQUAL '00'
              IF  WRK-FS-BXEXTANO   EQUAL '10'  AND
                  WRK-LEITURA       EQUAL WRK-MSG-FS-OPERACAO
                  NEXT  SENTENCE
              ELSE
                  MOVE 'APL'               TO ERR-TIPO-ACESSO
                  MOVE 'BXEXTANO'          TO WRK-MSG-FS-NOME-ARQ
                  MOVE WRK-FS-BXEXTANO     TO WRK-MSG-FS-CODIGO
                  MOVE WRK-MSG-FILE-STATUS TO ERR-TEXTO
                  MOVE 4                   TO WRK-RETURN-CODE
                  PERFORM 9999-ROTINA-ERRO
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       9220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9235-TESTAR-FS-EXTAVALI         SECTION.
      *----------------------------------------------------------------*

           IF WRK-FS-EXTAVALI          NOT EQUAL '00'
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE 'EXTAVALI'          TO WRK-MSG-FS-NOME-ARQ
              MOVE WRK-FS-EXTAVALI     TO WRK-MSG-FS-CODIGO
              MOVE WRK-MSG-FILE-STATUS TO ERR-TEXTO
              MOVE 4                   TO WRK-RETURN-CODE
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       9235-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT1826'             TO ERR-PGM.

           CALL 'BRAD7100'             USING WRK-BATCH
                                             ERRO-AREA.

           MOVE WRK-RETURN-CODE        TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
