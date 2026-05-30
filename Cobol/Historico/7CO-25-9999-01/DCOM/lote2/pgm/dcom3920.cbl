      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM3920.
       AUTHOR.     LUCIANDRA SILVEIRA.
      *================================================================*
      *                    BRQ DIGITAL SOLUTIONS                       *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM3920                                    *
      *    PROGRAMADOR.:   LUCIANDRA SILVEIRA - BRQ DIGITAL SOLUTIONS  *
      *    ANALISTA....:   LUCIANDRA SILVEIRA - BRQ DIGITAL SOLUTIONS  *
      *    DATA........:   12/12/2018                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   SIMULACAO DA OPERACAO - DEMONSTRATIVO FI-   *
      *                    NANCEIRO - IMPRESSAO.                       *
      *----------------------------------------------------------------*
      *    TELAS.......:   DCOMGDPM - SIMULACAO DA OPERACAO - DEMONTRA-*
      *                               TIVO FINANCEIRO - IMPRESSAO      *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA. *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO.    *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0703                                      *
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
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3920 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       77  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       77  WRK-MODNAME                 PIC  X(008)         VALUE SPACES.
       77  WRK-CONTPARM                PIC  9(005) COMP    VALUE ZEROS.
       77  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01 WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)         VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)         VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(011)         VALUE
              ' - LOCAL ='.
           05 WRK-LOCAL-ERRO           PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(002)         VALUE ' *'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMGDPM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-OUTPUT-GCPM.
           05 WRK-OUT-LL-GCPM          PIC S9(004) COMP    VALUE +0000.
           05 WRK-OUT-ZZ-GCPM          PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-OUT-DADOS-GCPM.
              10 WRK-OUT-NOME-GCPM     PIC  X(050)         VALUE SPACES.
              10 WRK-OUT-CNPJCPF-GCPM  PIC  X(019)         VALUE SPACES.
              10 WRK-OUT-BCAGCTA-GCPM  PIC  X(023)         VALUE SPACES.
              10 WRK-OUT-CPRODT-GCPM   PIC  9(003)         VALUE ZEROS.
              10 WRK-OUT-IPRODT-GCPM   PIC  X(012)         VALUE SPACES.
              10 WRK-OUT-CSPROD-GCPM   PIC  9(003)         VALUE ZEROS.
              10 WRK-OUT-ISPROD-GCPM   PIC  X(012)         VALUE SPACES.
              10 WRK-OUT-CMEIO-GCPM    PIC  9(003)         VALUE ZEROS.
              10 WRK-OUT-IMEIO-GCPM    PIC  X(015)         VALUE SPACES.
              10 WRK-OUT-TXDESC-GCPM   PIC  X(010)         VALUE SPACES.
              10 WRK-OUT-TXJURO-GCPM.
                 15 WRK-OUT-TXJURO-GCPM-N
                                       PIC  ZZ9,99999      VALUE ZEROS.
              10 WRK-OUT-TXFREQ-GCPM   PIC  X(004)         VALUE SPACES.
              10 WRK-OUT-TXBANDA-GCPM  PIC  X(025)         VALUE SPACES.
              10 WRK-OUT-TACSUBP-GCPM  PIC  X(001)         VALUE SPACES.
              10 WRK-OUT-TACFIXO-GCPM  PIC  X(001)         VALUE SPACES.
              10 WRK-OUT-VLRFIXO-GCPM.
                 15 WRK-OUT-VLRFIXO-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
              10 WRK-OUT-TACISEN-GCPM  PIC  X(001)         VALUE SPACES.
              10 WRK-OUT-TACVAR-GCPM   PIC  X(001)         VALUE SPACES.
              10 WRK-OUT-PERVAR-GCPM.
                 15 WRK-OUT-PERVAR-GCPM-N
                                       PIC  ZZ9,99         VALUE ZEROS.
              10 WRK-OUT-VLRMIN-GCPM.
                 15 WRK-OUT-VLRMIN-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
              10 WRK-OUT-VLRMAX-GCPM.
                 15 WRK-OUT-VLRMAX-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
              10 WRK-OUT-SIMULACAO-GCPM.
                 15 WRK-OUT-DTSIMUL-GCPM
                                       PIC  X(010)         VALUE SPACES.
                 15 WRK-OUT-DTFINAL-GCPM
                                       PIC  X(010)         VALUE SPACES.
                 15 WRK-OUT-VLSIMUL-GCPM.
                    20 WRK-OUT-VLSIMUL-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 15 WRK-OUT-DESCPRZ-GCPM
                                       PIC  X(015)         VALUE SPACES.
                 15 WRK-OUT-PZMEDIO-GCPM.
                    20 WRK-OUT-PZMEDIO-GCPM-N
                                       PIC  9(003)         VALUE ZEROS.
                 15 WRK-OUT-VTACSIM-GCPM.
                    20 WRK-OUT-VTACSIM-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                 15 WRK-OUT-QTDETIT-GCPM.
                    20 WRK-OUT-QTDETIT-GCPM-N
                                       PIC  9(006)         VALUE ZEROS.
                 15 WRK-OUT-VTACTIT-GCPM.
                    20 WRK-OUT-VTACTIT-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                 15 WRK-OUT-TXJURAM-GCPM.
                    20 WRK-OUT-TXJURAM-GCPM-N
                                       PIC  ZZ9,99999      VALUE ZEROS.
                 15 WRK-OUT-VLRJURO-GCPM.
                    20 WRK-OUT-VLRJURO-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                 15 WRK-OUT-TXJURAA-GCPM.
                    20 WRK-OUT-TXJURAA-GCPM-N
                                       PIC  ZZ9,99999      VALUE ZEROS.
                 15 WRK-OUT-VLRIOF-GCPM.
                    20 WRK-OUT-VLRIOF-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                 15 WRK-OUT-TXCETAM-GCPM.
                    20 WRK-OUT-TXCETAM-GCPM-N
                                       PIC  ZZZZ9,99999    VALUE ZEROS.
                 15 WRK-OUT-VLRLIQ-GCPM.
                    20 WRK-OUT-VLRLIQ-GCPM-N
                                       PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 15 WRK-OUT-TXCETAA-GCPM.
                    20 WRK-OUT-TXCETAA-GCPM-N
                                       PIC  ZZZZ9,99999    VALUE ZEROS.
              10 WRK-OUT-MENSA-GCPM    PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMGDPM - BRAD0660 ***'.
      *---------------------------------------------------------------*

       01  WRK-660-DCOMGDPM.
           05 WRK-660-LL-AREA-GCPM     PIC  9(004) COMP    VALUE  0000.
           05 WRK-660-LL-MENSA-GCPM    PIC  9(004) COMP    VALUE  0000.
           05 WRK-660-NOME-GCPM        PIC  9(004) COMP    VALUE  0050.
           05 WRK-660-CNPJCPF-GCPM     PIC  9(004) COMP    VALUE  0019.
           05 WRK-660-BCAGCTA-GCPM     PIC  9(004) COMP    VALUE  0023.
           05 WRK-660-CPRODT-GCPM      PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-IPRODT-GCPM      PIC  9(004) COMP    VALUE  0012.
           05 WRK-660-CSPROD-GCPM      PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-ISPROD-GCPM      PIC  9(004) COMP    VALUE  0012.
           05 WRK-660-CMEIO-GCPM       PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-IMEIO-GCPM       PIC  9(004) COMP    VALUE  0015.
           05 WRK-660-TXDESC-GCPM      PIC  9(004) COMP    VALUE  0010.
           05 WRK-660-TXJURO-GCPM      PIC  9(004) COMP    VALUE  0009.
           05 WRK-660-TXFREQ-GCPM      PIC  9(004) COMP    VALUE  0004.
           05 WRK-660-TXBANDA-GCPM     PIC  9(004) COMP    VALUE  0025.
           05 WRK-660-TACSUBP-GCPM     PIC  9(004) COMP    VALUE  0001.
           05 WRK-660-TACFIXO-GCPM     PIC  9(004) COMP    VALUE  0001.
           05 WRK-660-VLRFIXO-GCPM     PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-TACISEN-GCPM     PIC  9(004) COMP    VALUE  0001.
           05 WRK-660-TACVAR-GCPM      PIC  9(004) COMP    VALUE  0001.
           05 WRK-660-PERVAR-GCPM      PIC  9(004) COMP    VALUE  0006.
           05 WRK-660-VLRMIN-GCPM      PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-VLRMAX-GCPM      PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-DTSIMUL-GCPM     PIC  9(004) COMP    VALUE  0010.
           05 WRK-660-DTFINAL-GCPM     PIC  9(004) COMP    VALUE  0010.
           05 WRK-660-VLSIMUL-GCPM     PIC  9(004) COMP    VALUE  0018.
           05 WRK-660-DESCPRZ-GCPM     PIC  9(004) COMP    VALUE  0015.
           05 WRK-660-PZMEIO-GCPM      PIC  9(004) COMP    VALUE  0003.
           05 WRK-660-VTACSIM-GCPM     PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-QTDETIT-GCPM     PIC  9(004) COMP    VALUE  0006.
           05 WRK-660-VTACTIT-GCPM     PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-TXJURAM-GCPM     PIC  9(004) COMP    VALUE  0009.
           05 WRK-660-VLRJURO-GCPM     PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-TXJURAA-GCPM     PIC  9(004) COMP    VALUE  0009.
           05 WRK-660-VLRIOF-GCPM      PIC  9(004) COMP    VALUE  0014.
           05 WRK-660-TXCETAM-GCPM     PIC  9(004) COMP    VALUE  0011.
           05 WRK-660-VLRLIQ-GCPM      PIC  9(004) COMP    VALUE  0018.
           05 WRK-660-TXCETAA-GCPM     PIC  9(004) COMP    VALUE  0011.
           05 WRK-660-MENSA-GCPM       PIC  9(004) COMP    VALUE  0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM3920 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

       01  LNK-AREA-DCOM3920.
           05 LNK-COD-RETORNO          PIC  9(002).
           05 LNK-IMPRESSORA           PIC  X(008).
           05 LNK-USUARIO              PIC  X(007).
           05 LNK-DEPTO                PIC  X(006).
           05 LNK-NOME                 PIC  X(050).
           05 LNK-CNPJCPF              PIC  X(019).
           05 LNK-BCAGCTA              PIC  X(023).
           05 LNK-CPRODT               PIC  9(003).
           05 LNK-IPRODT               PIC  X(012).
           05 LNK-CSPROD               PIC  9(003).
           05 LNK-ISPROD               PIC  X(012).
           05 LNK-CMEIO                PIC  9(003).
           05 LNK-IMEIO                PIC  X(015).
           05 LNK-TXDESC               PIC  X(010).
           05 LNK-TXJURO               PIC  X(009).
           05 LNK-TXFREQ               PIC  X(004).
           05 LNK-TXBANDA              PIC  X(025).
           05 LNK-TACSUBP              PIC  X(001).
           05 LNK-TACFIXO              PIC  X(001).
           05 LNK-VLRFIXO              PIC  X(014).
           05 LNK-TACISEN              PIC  X(001).
           05 LNK-TACVAR               PIC  X(001).
           05 LNK-PERVAR               PIC  X(006).
           05 LNK-VLRMIN               PIC  X(014).
           05 LNK-VLRMAX               PIC  X(014).
           05 LNK-DTSIMUL              PIC  X(010).
           05 LNK-DTFINAL              PIC  X(010).
           05 LNK-VLSIMUL              PIC  X(018).
           05 LNK-DESCPRZ              PIC  X(015).
           05 LNK-PZMEDIO              PIC  X(003).
           05 LNK-VTACSIM              PIC  X(014).
           05 LNK-QTDETIT              PIC  X(006).
           05 LNK-VTACTIT              PIC  X(014).
           05 LNK-TXJURAM              PIC  X(009).
           05 LNK-VLRJURO              PIC  X(014).
           05 LNK-TXJURAA              PIC  X(009).
           05 LNK-VLRIOF               PIC  X(014).
           05 LNK-TXCETAM              PIC  X(011).
           05 LNK-VLRLIQ               PIC  X(018).
           05 LNK-TXCETAA              PIC  X(011).

      *================================================================*
       PROCEDURE DIVISION              USING LNK-IO-PCB
                                             LNK-ALT-PCB
                                             LNK-AREA-DCOM3920.
      *================================================================*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'CBLTDLI'              USING WRK-CHNG
                                             LNK-ALT-PCB
                                             LNK-IMPRESSORA.

           IF  LNK-ALT-STATUS          EQUAL 'A1'
               MOVE 02                 TO LNK-COD-RETORNO
               PERFORM 2000-FINALIZAR
           END-IF.

           IF  LNK-ALT-STATUS          NOT EQUAL SPACES
               MOVE 'IMS'              TO ERR-TIPO-ACESSO
               MOVE LNK-IMPRESSORA     TO ERR-DBD-TAB
               MOVE WRK-CHNG           TO ERR-FUN-COMANDO
               MOVE LNK-ALT-STATUS     TO ERR-STA-CODE
               MOVE '0010'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE ZEROS                  TO LNK-COD-RETORNO.
           MOVE SPACES                 TO WRK-OUT-DADOS-GCPM.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           PERFORM 2000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE LNK-NOME               TO WRK-OUT-NOME-GCPM
           MOVE LNK-CNPJCPF            TO WRK-OUT-CNPJCPF-GCPM
           MOVE LNK-BCAGCTA            TO WRK-OUT-BCAGCTA-GCPM
           MOVE LNK-CPRODT             TO WRK-OUT-CPRODT-GCPM
           MOVE LNK-IPRODT             TO WRK-OUT-IPRODT-GCPM
           MOVE LNK-CSPROD             TO WRK-OUT-CSPROD-GCPM
           MOVE LNK-ISPROD             TO WRK-OUT-ISPROD-GCPM
           MOVE LNK-CMEIO              TO WRK-OUT-CMEIO-GCPM
           MOVE LNK-IMEIO              TO WRK-OUT-IMEIO-GCPM
           MOVE LNK-TXDESC             TO WRK-OUT-TXDESC-GCPM
           MOVE LNK-TXJURO             TO WRK-OUT-TXJURO-GCPM.
           MOVE LNK-TXFREQ             TO WRK-OUT-TXFREQ-GCPM
           MOVE LNK-TXBANDA            TO WRK-OUT-TXBANDA-GCPM
           MOVE LNK-TACSUBP            TO WRK-OUT-TACSUBP-GCPM
           MOVE LNK-TACFIXO            TO WRK-OUT-TACFIXO-GCPM
           MOVE LNK-VLRFIXO            TO WRK-OUT-VLRFIXO-GCPM.
           MOVE LNK-TACISEN            TO WRK-OUT-TACISEN-GCPM
           MOVE LNK-TACVAR             TO WRK-OUT-TACVAR-GCPM
           MOVE LNK-PERVAR             TO WRK-OUT-PERVAR-GCPM.
           MOVE LNK-VLRMIN             TO WRK-OUT-VLRMIN-GCPM.
           MOVE LNK-VLRMAX             TO WRK-OUT-VLRMAX-GCPM
           MOVE LNK-DTSIMUL            TO WRK-OUT-DTSIMUL-GCPM
           MOVE LNK-DTFINAL            TO WRK-OUT-DTFINAL-GCPM
           MOVE LNK-VLSIMUL            TO WRK-OUT-VLSIMUL-GCPM
           MOVE LNK-DESCPRZ            TO WRK-OUT-DESCPRZ-GCPM
           MOVE LNK-PZMEDIO            TO WRK-OUT-PZMEDIO-GCPM
           MOVE LNK-VTACSIM            TO WRK-OUT-VTACSIM-GCPM
           MOVE LNK-QTDETIT            TO WRK-OUT-QTDETIT-GCPM
           MOVE LNK-VTACTIT            TO WRK-OUT-VTACTIT-GCPM
           MOVE LNK-TXJURAM            TO WRK-OUT-TXJURAM-GCPM
           MOVE LNK-VLRJURO            TO WRK-OUT-VLRJURO-GCPM
           MOVE LNK-TXJURAA            TO WRK-OUT-TXJURAA-GCPM
           MOVE LNK-VLRIOF             TO WRK-OUT-VLRIOF-GCPM
           MOVE LNK-TXCETAM            TO WRK-OUT-TXCETAM-GCPM
           MOVE LNK-VLRLIQ             TO WRK-OUT-VLRLIQ-GCPM
           MOVE LNK-TXCETAA            TO WRK-OUT-TXCETAA-GCPM.

           PERFORM 1100-PROCESSAR-BRAD0660.

           PERFORM 1200-INSERIR-TELA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1100-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-GCPM
                                       TO WRK-OUT-LL-GCPM
                                          WRK-660-LL-MENSA-GCPM.
           MOVE LENGTH                 OF WRK-660-DCOMGDPM
                                       TO WRK-660-LL-AREA-GCPM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-GCPM
                                             WRK-660-DCOMGDPM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     INSERE OS SEGMENTOS DA IMPRESSAO                           *
      *----------------------------------------------------------------*
       1200-INSERIR-TELA               SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMGDPM'             TO WRK-MODNAME.
           MOVE 0004                   TO WRK-CONTPARM.

           CALL 'CBLTDLI'              USING WRK-CONTPARM
                                             WRK-ISRT
                                             LNK-ALT-PCB
                                             WRK-OUTPUT-GCPM
                                             WRK-MODNAME.

           IF  LNK-ALT-STATUS          NOT EQUAL SPACES
               MOVE 'IMS'              TO ERR-TIPO-ACESSO
               MOVE WRK-MODNAME        TO ERR-DBD-TAB
               MOVE WRK-ISRT           TO ERR-FUN-COMANDO
               MOVE LNK-ALT-STATUS     TO ERR-STA-CODE
               MOVE '0030'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     INSERE OS SEGMENTOS DA IMPRESSAO                           *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAMENTO DE ERRO                                      *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM0703'         TO ERR-PGM
           ELSE
               MOVE 'DCOM0703'         TO ERR-PGM
               MOVE 'DCOM3920'         TO ERR-MODULO
           END-IF.

           MOVE LNK-USUARIO            TO ERR-COD-USER.
           MOVE LNK-DEPTO              TO ERR-COD-DEPTO.

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           MOVE 04                     TO LNK-COD-RETORNO

           PERFORM 2000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
