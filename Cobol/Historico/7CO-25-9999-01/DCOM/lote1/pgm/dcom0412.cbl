      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0412.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0412                                    *
      *    PROGRAMADOR.:   DAVI BRAZ                                   *
      *    DATA........:   13/08/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   RESUMO ANALISE                              *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDLTM - RESUMO ANALISE                                   *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0405                                      *
      *                  DCOM0408                                      *
      *                  DCOM0411                                      *
      *================================================================*
DTS001*                   ALTERACAO  -  DTS  CONSULTING                *
DTS001*----------------------------------------------------------------*
DTS001*     ANALISTA DTS: LUCIANDRA SILVEIRA      -  DTS CONSULTING    *
DTS001*     ANALISTA DDS: RICARDO JAMMAL          -  BRADESCO - GP. 70 *
DTS001*     DATA........: 10/12/2012                                   *
DTS001*----------------------------------------------------------------*
DTS001*     OBJETIVO....: ADEQUAR PROGRAMA PARA ATENDER NOVOS NIVEIS DE*
DTS001*                   RATING. PMO 12/0129 - NIVEIS DE RATING.      *
DTS001*================================================================*

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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0412 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-IND                 PIC  9(002) COMP-3  VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-FLAG-ERRO           PIC  X(001)         VALUE 'N'.
           05  WRK-MENSAGEM-ERRO       PIC  X(079)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)         VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)         VALUE SPACES.
           05  WRK-TELA                PIC  X(008)         VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)         VALUE
               'VRS002'.
           05  WRK-COD-USER            PIC  X(007)         VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)         VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)         VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDLTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  INPUT-DLTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  INP-PFK-DLTM            PIC  X(001) VALUE SPACES.
           05  INP-SENHAS-DLTM         PIC  X(037) VALUE SPACES.
           05  INP-FIMAMOS-DLTM        PIC  X(001) VALUE SPACES.
           05  INP-PAGINA-DLTM         PIC  9(003) VALUE ZEROS.
           05  INP-COMANDO-DLTM        PIC  X(068) VALUE SPACES.
           05  INP-TRANSAC-DLTM        PIC  X(008) VALUE SPACES.
           05  INP-CHAMADO-DLTM        PIC  X(008) VALUE SPACES.
           05  INP-FLAGENT-DLTM        PIC  X(001) VALUE SPACES.
           05  INP-PRLIBER-DLTM        PIC  9(003)V99 VALUE ZEROS.
           05  INP-VLLIBER-DLTM        PIC  9(015)V99 VALUE ZEROS.
           05  INP-FASE-DLTM           PIC  X(001) VALUE SPACES.
           05  INP-OPCAO-DLTM          PIC  X(001) VALUE SPACES.
           05  INP-NOMECLI-DLTM        PIC  X(040) VALUE SPACES.
           05  INP-CNPJCPF-DLTM        PIC  X(019) VALUE SPACES.
           05  INP-CAMPO2-DLTM         PIC  X(023) VALUE SPACES.
           05  INP-PRODUTO-DLTM        PIC  9(003) VALUE ZEROS.
           05  INP-DPRODUT-DLTM        PIC  X(012) VALUE SPACES.
           05  INP-SPRODUT-DLTM        PIC  9(003) VALUE ZEROS.
           05  INP-DSPRODU-DLTM        PIC  X(010) VALUE SPACES.
           05  INP-OPERAC-DLTM         PIC  9(013) VALUE ZEROS.
           05  INP-SITUAC-DLTM         PIC  X(015) VALUE SPACES.
           05  INP-CAMPO3-DLTM         PIC  X(009) VALUE SPACES.
           05  INP-CAMPO4-DLTM         PIC  9(009) VALUE ZEROS.
           05  INP-TSTAMP-DLTM         PIC  X(026) VALUE SPACES.
           05  INP-OCOR-DLTM           OCCURS 4 TIMES.
               10  INP-NOME-DLTM       PIC  X(020) VALUE SPACES.
               10  INP-RESTR-DLTM      PIC  X(003) VALUE SPACES.
               10  INP-CNPJ-DLTM       PIC  X(019) VALUE SPACES.
               10  INP-CANC-DLTM       PIC  X(001) VALUE SPACES.
               10  INP-RATING-DLTM     PIC  X(006) VALUE SPACES.
DTS001         10  WRK-INP-NIVEL-DLTM  PIC  X(002) VALUE SPACES.
               10  INP-CEP-DLTM        PIC  X(001) VALUE SPACES.
               10  INP-LIQSAC-DLTM     PIC  9(004) VALUE ZEROS.
               10  INP-PERC1-DLTM      PIC  X(001) VALUE SPACES.
               10  INP-CEDSAC-DLTM     PIC  9(004) VALUE ZEROS.
               10  INP-PERC2-DLTM      PIC  X(001) VALUE SPACES.
               10  INP-SACOPER-DLTM    PIC  9(004) VALUE ZEROS.
               10  INP-PERC3-DLTM      PIC  X(001) VALUE SPACES.
           05  INP-MENSA-DLTM          PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDLTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  OUTPUT-DLTM.
           05  OUT-LL-DLTM             PIC S9(004) COMP VALUE +0675.
           05  OUT-ZZ-DLTM             PIC  9(004) COMP VALUE ZEROS.
           05  OUT-SENHAS-DLTM         PIC  X(037) VALUE SPACES.
           05  OUT-FIMAMOS-DLTM        PIC  X(001) VALUE SPACES.
           05  OUT-PAGINA-DLTM         PIC  9(003) VALUE ZEROS.
           05  OUT-COMANDO-DLTM        PIC  X(068) VALUE SPACES.
           05  OUT-TRANSAC-DLTM        PIC  X(008) VALUE SPACES.
           05  OUT-CHAMADO-DLTM        PIC  X(008) VALUE SPACES.
           05  OUT-FLAGENT-DLTM        PIC  X(001) VALUE SPACES.
           05  OUT-PRLIBER-DLTM        PIC  9(003)V99 VALUE ZEROS.
           05  OUT-VLLIBER-DLTM        PIC  9(015)V99 VALUE ZEROS.
           05  OUT-FASE-DLTM           PIC  X(001) VALUE SPACES.
           05  OUT-OPCAO-DLTM          PIC  X(001) VALUE SPACES.
           05  OUT-NOMECLI-DLTM        PIC  X(040) VALUE SPACES.
           05  OUT-CNPJCPF-DLTM        PIC  X(019) VALUE SPACES.
           05  OUT-CAMPO2-DLTM         PIC  X(023) VALUE SPACES.
           05  OUT-PRODUTO-DLTM        PIC  9(003) VALUE ZEROS.
           05  OUT-DPRODUT-DLTM        PIC  X(012) VALUE SPACES.
           05  OUT-SPRODUT-DLTM        PIC  9(003) VALUE ZEROS.
           05  OUT-DSPRODU-DLTM        PIC  X(010) VALUE SPACES.
           05  OUT-OPERAC-DLTM         PIC  9(013) VALUE ZEROS.
           05  OUT-SITUAC-DLTM         PIC  X(015) VALUE SPACES.
           05  OUT-CAMPO3-DLTM         PIC  X(009) VALUE SPACES.
           05  OUT-CAMPO4-DLTM.
               10 OUT-CAMPO4-DLTM-N    PIC  9(009) VALUE ZEROS.
           05  OUT-TSTAMP-DLTM         PIC  X(026) VALUE SPACES.
           05  OUT-OCOR-DLTM           OCCURS 4 TIMES.
               10  OUT-NOME-DLTM       PIC  X(020) VALUE SPACES.
               10  OUT-RESTR-DLTM      PIC  X(003) VALUE SPACES.
               10  OUT-CNPJ-DLTM       PIC  X(019) VALUE SPACES.
               10  OUT-CANC-DLTM       PIC  X(001) VALUE SPACES.
               10  OUT-RATING-DLTM     PIC  X(006) VALUE SPACES.
DTS001         10  WRK-OUT-NIVEL-DLTM-X.
DTS001             15  WRK-OUT-NIVEL-DLTM
DTS001                                 PIC  9(002) VALUE ZEROS.
               10  OUT-CEP-DLTM        PIC  X(001) VALUE SPACES.
               10  OUT-LIQSAC-DLTM-X.
                   15  OUT-LIQSAC-DLTM PIC  9,99   VALUE ZEROS.
               10  OUT-PERC1-DLTM      PIC  X(001) VALUE SPACES.
               10  OUT-CEDSAC-DLTM-X.
                   15  OUT-CEDSAC-DLTM PIC  9,99   VALUE ZEROS.
               10  OUT-PERC2-DLTM      PIC  X(001) VALUE SPACES.
               10  OUT-SACOPER-DLTM-X.
                   15  OUT-SACOPER-DLTM
                                       PIC  9,99   VALUE ZEROS.
               10  OUT-PERC3-DLTM      PIC  X(001) VALUE SPACES.
           05  OUT-MENSA-DLTM          PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDLTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
      *  AREA DA BRAD0660 COM MASCARA DA TELA DCOMDLTM - SEGMENTO 00  *
      *---------------------------------------------------------------*
       01  WRK-660-DCOMDLTM.
           05  660-LL-AREA-DLTM        PIC  9(04)  COMP  VALUE 0148.
           05  660-LL-MENSAGEM-DLTM    PIC  9(04)  COMP  VALUE 0675.
           05  660-SENHAS-DLTM         PIC  9(04)  COMP  VALUE 0037.
           05  660-FIMAMOS-DLTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-PAGINA-DLTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-COMANDO-DLTM        PIC  9(04)  COMP  VALUE 0068.
           05  660-TRANSAC-DLTM        PIC  9(04)  COMP  VALUE 0008.
           05  660-CHAMADO-DLTM        PIC  9(04)  COMP  VALUE 0008.
           05  660-FLAGENT-DLTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-PRLIBER-DLTM        PIC  9(04)  COMP  VALUE 0005.
           05  660-VLLIBER-DLTM        PIC  9(04)  COMP  VALUE 0017.
           05  660-FASE-DLTM           PIC  9(04)  COMP  VALUE 0001.
           05  660-OPCAO-DLTM          PIC  9(04)  COMP  VALUE 0001.
           05  660-NOMECLI-DLTM        PIC  9(04)  COMP  VALUE 0040.
           05  660-CNPJCPF-DLTM        PIC  9(04)  COMP  VALUE 0019.
           05  660-CAMPO2-DLTM         PIC  9(04)  COMP  VALUE 0023.
           05  660-PRODUTO-DLTM        PIC  9(04)  COMP  VALUE 0003.
           05  660-DPRODUT-DLTM        PIC  9(04)  COMP  VALUE 0012.
           05  660-SPRODUT-DLTM        PIC  9(04)  COMP  VALUE 0003.
           05  660-DSPRODU-DLTM        PIC  9(04)  COMP  VALUE 0010.
           05  660-OPERAC-DLTM         PIC  9(04)  COMP  VALUE 0013.
           05  660-SITUAC-DLTM         PIC  9(04)  COMP  VALUE 0015.
           05  660-CAMPO3-DLTM         PIC  9(04)  COMP  VALUE 0009.
           05  660-CAMPO4-DLTM         PIC  9(04)  COMP  VALUE 0009.
           05  660-TSTAMP-DLTM         PIC  9(04)  COMP  VALUE 0026.
           05  660-OCOR-DLTM           OCCURS 4 TIMES.
               10  660-NOME-DLTM       PIC  9(04)  COMP  VALUE 0020.
               10  660-RESTR-DLTM      PIC  9(04)  COMP  VALUE 0003.
               10  660-CNPJ-DLTM       PIC  9(04)  COMP  VALUE 0019.
               10  660-CANC-DLTM       PIC  9(04)  COMP  VALUE 0001.
               10  660-RATING-DLTM     PIC  9(04)  COMP  VALUE 0006.
DTS001         10  WRK-660-NIVEL-DLTM  PIC  9(04)  COMP  VALUE 0002.
               10  660-CEP-DLTM        PIC  9(04)  COMP  VALUE 0001.
               10  660-LIQSAC-DLTM     PIC  9(04)  COMP  VALUE 0004.
               10  660-PERC1-DLTM      PIC  9(04)  COMP  VALUE 0001.
               10  660-CEDSAC-DLTM     PIC  9(04)  COMP  VALUE 0004.
               10  660-PERC2-DLTM      PIC  9(04)  COMP  VALUE 0001.
               10  660-SACOPER-DLTM    PIC  9(04)  COMP  VALUE 0004.
               10  660-PERC3-DLTM      PIC  9(04)  COMP  VALUE 0001.
           05  660-MENSA-DLTM          PIC  9(04)  COMP  VALUE 0079.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO **'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP VALUE ZEROS.
           05  COMU-ZZ                 PIC S9(004) COMP VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)   VALUE SPACES.
               10  FILLER              PIC  X(006)   VALUE SPACES.
               10  COMU-PFK            PIC  X(001)   VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)   VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)   VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPERACAO       PIC  9(13)          VALUE ZEROS.
               10  COMU-TRANSA-ANTE    PIC  X(08)          VALUE SPACES.
               10  COMU-FLAGENT        PIC  X(01)          VALUE SPACES.
               10  COMU-FASE           PIC  X(01)          VALUE SPACES.
               10  COMU-PRLIBER        PIC  9(03)V99       VALUE ZEROS.
               10  COMU-VLLIBER        PIC  9(13)V99       VALUE ZEROS.
               10  COMU-OPCAO          PIC  X(01)          VALUE SPACES.
               10  COMU-MENSAGEM       PIC X(79)           VALUE SPACES.
               10  COMU-TMESTAMP       PIC X(26)           VALUE SPACES.

      *----------------------------------------------------------------*

       01  4230-ENTRADA-ROTEADOR.
           03  4230E-HEADER.
               05  4230E-COD-RETORNO           PIC  X(004).
               05  4230E-MSG-RETORNO           PIC  X(079).
               05  4230E-RESTART               PIC  9(005).
               05  4230E-FLAG                  PIC  X(001).
               05  4230E-START-COUNT           PIC  X(001).
               05  4230E-CFUNC-BDSCO           PIC  9(009).
               05  4230E-CTERM                 PIC  X(008).
           03  4230E-CONSISTENCIA.
               05  4230E-OPERACAO              PIC  9(013).
               05  FILLER REDEFINES 4230E-OPERACAO.
                07 4230E-DANO-OPER-DESC        PIC  9(004).
                07 4230E-NSEQ-OPER-DESC        PIC  9(009).

       01  4230-SAIDA-ROTEADOR.
           03  4230S-HEADER.
               05  4230S-COD-RETORNO           PIC  X(004).
               05  4230S-MSG-RETORNO           PIC  X(079).
               05  4230S-RESTART               PIC  9(05).
               05  4230S-FLAG                  PIC  X(01).
               05  4230S-QTDE-RETORNADA        PIC  9(03).
               05  4230S-COUNT                 PIC  9(09).
           03  4230S-DADOS.
               05  4230S-RESTRICOES    OCCURS 20 TIMES
                                        INDEXED BY IND-LKG.
                   07 4230S-IPSSOA-SACDO       PIC  X(30).
FS2511*            07 4230S-CCNPJ-CPF          PIC  9(09).
FS2511*            07 4230S-CFLIAL-CNPJ        PIC  9(004).
FS2511             07 4230S-CCNPJ-CPF          PIC  X(09).
FS2511             07 4230S-CFLIAL-CNPJ        PIC  X(04).
                   07 4230S-CCTRL-CNPJ-CPF     PIC  9(02).
                   07 4230S-IRES               PIC  X(06).
                   07 4230S-CNSE               PIC  X(06).
                   07 4230S-RATING             PIC  X(06).
DTS001             07 WRK-4230S-NNVEL-RTING
DTS001                                 PIC  9(002)        VALUE ZEROS.
DTS001             07 WRK-4230S-HINIC-VGCIA-NVEL
DTS001                                 PIC  X(026)        VALUE SPACES.
                   07 4230S-CEP                PIC  X(06).
                   07 4230S-GRUP               PIC  X(06).
                   07 4230S-SLIG               PIC  9(02)V99.
                   07 4230S-CONCENTRACAO       PIC  9(02)V99.
                   07 4230S-CONCEN-SACADO      PIC  9(03)V99.

           03  4230S-CONSISTENCIA      REDEFINES 4230S-DADOS.
               05  4230S-TAMANHO-ERRO          PIC  9(001).
               05  4230S-TABELA-ERROS          OCCURS 002 TIMES.
                   07  4230S-CAMPOS-ERROS      PIC  9(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY I#DCOMXJ.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0412 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE          EQUAL  04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMDLTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    NOT EQUAL  'DCOM0412'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDLTM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDLTM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DE OUTRAS TRANSACOES            *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-TRANSACAO          TO INP-CHAMADO-DLTM
                                          OUT-CHAMADO-DLTM.
           MOVE WRK-MENSAGEM           TO COMU-AREA.
           MOVE 1                      TO OUT-PAGINA-DLTM.

           INITIALIZE                  4174-ENTRADA-ROTEADOR
                                       4174-SAIDA-ROTEADOR
                                       4230-ENTRADA-ROTEADOR
                                       4230-SAIDA-ROTEADOR.

           MOVE COMU-FASE              TO OUT-FASE-DLTM
                                          INP-FASE-DLTM
           MOVE COMU-TMESTAMP          TO OUT-TSTAMP-DLTM
                                          INP-TSTAMP-DLTM
           MOVE COMU-PRLIBER           TO OUT-PRLIBER-DLTM
                                          INP-PRLIBER-DLTM
           MOVE COMU-VLLIBER           TO OUT-VLLIBER-DLTM
                                          INP-VLLIBER-DLTM
           MOVE COMU-TRANSA-ANTE       TO OUT-TRANSAC-DLTM
                                          INP-TRANSAC-DLTM
           MOVE COMU-FLAGENT           TO OUT-FLAGENT-DLTM
                                          INP-FLAGENT-DLTM
           MOVE COMU-OPCAO             TO OUT-OPCAO-DLTM
                                          INP-OPCAO-DLTM

           PERFORM 1105-ACESSAR-DCOM4174.

           MOVE ZEROS                  TO 4230E-RESTART
           MOVE COMU-OPERACAO          TO 4230E-OPERACAO
                                          INP-OPERAC-DLTM
                                          OUT-OPERAC-DLTM

           PERFORM 1110-ACESSAR-DCOM4230.

           MOVE 'TECLE <PF3> PARA VOLTAR'
                                       TO OUT-MENSA-DLTM.


      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1105-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM
           MOVE ZEROS                  TO 4174-ENT-RESTART
           MOVE SPACES                 TO 4174-ENT-FLAG
           MOVE COMU-OPERACAO(1:4)     TO 4174-ENT-DANO-OPER-DESC
           MOVE COMU-OPERACAO(5:9)     TO 4174-ENT-NSEQ-OPER-DESC
           MOVE 'DCOM4174'             TO WRK-MODULO

           CALL WRK-MODULO    USING    4174-ENTRADA-ROTEADOR
                                       4174-SAIDA-ROTEADOR
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO
               WHEN '0000'

           MOVE 4174-SAI-IPSSOA-DESC-COML  TO OUT-NOMECLI-DLTM
           STRING 4174-SAI-CCNPJ-CPF(1:3)
           '.'    4174-SAI-CCNPJ-CPF(4:3)
           '.'    4174-SAI-CCNPJ-CPF(7:3)
           '/'    4174-SAI-CFLIAL-CNPJ
           '-'    4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE               INTO OUT-CNPJCPF-DLTM
           STRING 4174-SAI-CBCO
           '/'    4174-SAI-CAG-BCRIA
           '/'    4174-SAI-CCTA-BCRIA-CLI
           DELIMITED BY SIZE               INTO OUT-CAMPO2-DLTM
           MOVE 4174-SAI-CPRODT            TO OUT-PRODUTO-DLTM
           MOVE 4174-SAI-IABREV-PRODT      TO OUT-DPRODUT-DLTM
           MOVE 4174-SAI-CSPROD-DESC-COML  TO OUT-SPRODUT-DLTM
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO OUT-DSPRODU-DLTM
           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO OUT-SITUAC-DLTM
           MOVE SPACES                     TO OUT-CAMPO3-DLTM
           MOVE SPACES                     TO OUT-CAMPO4-DLTM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       1105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4230           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO 4230E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4230E-CTERM
           MOVE 'N'                    TO 4230E-START-COUNT
           MOVE SPACES                 TO 4230E-FLAG
           MOVE 'DCOM4230'             TO WRK-MODULO

           CALL WRK-MODULO    USING    4230-ENTRADA-ROTEADOR
                                       4230-SAIDA-ROTEADOR
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE 4230S-COD-RETORNO
               WHEN '0000'
FS2511*           IF 4230S-CCNPJ-CPF(5) EQUAL ZEROS
                    IF 4230S-CCNPJ-CPF(5) EQUAL SPACES OR
                       4230S-CCNPJ-CPF(5) EQUAL LOW-VALUES
                       MOVE '*'     TO  OUT-FIMAMOS-DLTM
                    ELSE
                       MOVE SPACES  TO  OUT-FIMAMOS-DLTM
                    END-IF
                    PERFORM 1120-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 4

               WHEN '0003'
                    MOVE 'RESULTADO DA ANALISE NAO ENCONTRADO'
                                    TO  WRK-MENSAGEM-ERRO
                    MOVE 'S'        TO  WRK-FLAG-ERRO
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE 4230S-MSG-RETORNO
                                    TO  WRK-MENSAGEM-ERRO
                    MOVE 'S'        TO  WRK-FLAG-ERRO
                    PERFORM 1220-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1120-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

FS2511*    IF 4230S-CCNPJ-CPF(WRK-IND) IS NUMERIC
FS2511*    AND 4230S-CCNPJ-CPF(WRK-IND) NOT EQUAL ZEROS
           IF 4230S-CCNPJ-CPF(WRK-IND) NOT EQUAL SPACES AND
              4230S-CCNPJ-CPF(WRK-IND) NOT EQUAL LOW-VALUES
               MOVE 4230S-IPSSOA-SACDO(WRK-IND)
                                       TO OUT-NOME-DLTM(WRK-IND)
               IF  4230S-IRES(WRK-IND) EQUAL 'S'
                   MOVE 'SIM'          TO OUT-RESTR-DLTM(WRK-IND)
               ELSE
                   MOVE 'NAO'          TO OUT-RESTR-DLTM(WRK-IND)
               END-IF
               STRING 4230S-CCNPJ-CPF(WRK-IND)(1:3)
               '.'    4230S-CCNPJ-CPF(WRK-IND)(4:3)
               '.'    4230S-CCNPJ-CPF(WRK-IND)(7:3)
FS2511*              '/'    4230S-CFLIAL-CNPJ(WRK-IND)(2:4)
               '/'    4230S-CFLIAL-CNPJ(WRK-IND)(1:4)
               '-'    4230S-CCTRL-CNPJ-CPF(WRK-IND)
               DELIMITED BY SIZE       INTO OUT-CNPJ-DLTM(WRK-IND)
               MOVE 4230S-RATING(WRK-IND)
                                       TO OUT-RATING-DLTM(WRK-IND)
DTS001         MOVE WRK-4230S-NNVEL-RTING
DTS001                       (WRK-IND) TO WRK-OUT-NIVEL-DLTM(WRK-IND)
               MOVE 4230S-CEP(WRK-IND) TO OUT-CEP-DLTM(WRK-IND)
               MOVE 4230S-SLIG(WRK-IND)
                                       TO OUT-LIQSAC-DLTM(WRK-IND)
               MOVE 4230S-CONCENTRACAO(WRK-IND)
                                       TO OUT-CEDSAC-DLTM(WRK-IND)
               MOVE 4230S-CONCEN-SACADO(WRK-IND)
                                       TO OUT-SACOPER-DLTM(WRK-IND)
               MOVE 4230S-CNSE(WRK-IND)TO OUT-CANC-DLTM(WRK-IND)
               MOVE '%'                TO OUT-PERC1-DLTM(WRK-IND)
                                          OUT-PERC2-DLTM(WRK-IND)
                                          OUT-PERC3-DLTM(WRK-IND)
           ELSE
               MOVE SPACES             TO OUT-OCOR-DLTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDLTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDLTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO INPUT-DLTM.
           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN INP-PFK-DLTM     EQUAL 'H' OR
                   (INP-PFK-DLTM     EQUAL '.' AND
                    INP-COMANDO-DLTM EQUAL 'PFK01')
                    MOVE INP-MENSA-DLTM TO OUT-MENSA-DLTM

               WHEN INP-PFK-DLTM     EQUAL '3' OR
                   (INP-PFK-DLTM     EQUAL '.' AND
                    INP-COMANDO-DLTM EQUAL 'PFK03')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN INP-PFK-DLTM     EQUAL 'X' OR
                   (INP-PFK-DLTM     EQUAL '.' AND
                    INP-COMANDO-DLTM EQUAL 'PFK05')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN INP-PFK-DLTM     EQUAL 'Z' OR
                   (INP-PFK-DLTM     EQUAL '.' AND
                    INP-COMANDO-DLTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN INP-PFK-DLTM     EQUAL '7' OR
                   (INP-PFK-DLTM     EQUAL '.' AND
                    INP-COMANDO-DLTM EQUAL 'PFK07')
                    PERFORM 1250-TRATAR-VOLTA-PAG

               WHEN INP-PFK-DLTM     EQUAL '8' OR
                   (INP-PFK-DLTM     EQUAL '.' AND
                    INP-COMANDO-DLTM EQUAL 'PFK08')
                    PERFORM 1260-TRATAR-AVANCA-PAG

               WHEN INP-PFK-DLTM     EQUAL '.' AND
                    INP-COMANDO-DLTM EQUAL  SPACES OR LOW-VALUES
                    MOVE 'TECLE <PF3> PARA VOLTAR'
                                         TO OUT-MENSA-DLTM

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO OUT-MENSA-DLTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE INP-SENHAS-DLTM        TO OUT-SENHAS-DLTM
           MOVE INP-FIMAMOS-DLTM       TO OUT-FIMAMOS-DLTM
           MOVE INP-PAGINA-DLTM        TO OUT-PAGINA-DLTM
           MOVE INP-COMANDO-DLTM       TO OUT-COMANDO-DLTM
           MOVE INP-CHAMADO-DLTM       TO OUT-CHAMADO-DLTM
           MOVE INP-FLAGENT-DLTM       TO OUT-FLAGENT-DLTM
           MOVE INP-PRLIBER-DLTM       TO OUT-PRLIBER-DLTM
           MOVE INP-VLLIBER-DLTM       TO OUT-VLLIBER-DLTM
           MOVE INP-FASE-DLTM          TO OUT-FASE-DLTM
           MOVE INP-NOMECLI-DLTM       TO OUT-NOMECLI-DLTM
           MOVE INP-CNPJCPF-DLTM       TO OUT-CNPJCPF-DLTM
           MOVE INP-CAMPO2-DLTM        TO OUT-CAMPO2-DLTM
           MOVE INP-PRODUTO-DLTM       TO OUT-PRODUTO-DLTM
           MOVE INP-DPRODUT-DLTM       TO OUT-DPRODUT-DLTM
           MOVE INP-SPRODUT-DLTM       TO OUT-SPRODUT-DLTM
           MOVE INP-DSPRODU-DLTM       TO OUT-DSPRODU-DLTM
           MOVE INP-OPERAC-DLTM        TO OUT-OPERAC-DLTM
           MOVE INP-SITUAC-DLTM        TO OUT-SITUAC-DLTM
           MOVE INP-CAMPO3-DLTM        TO OUT-CAMPO3-DLTM
           MOVE INP-TSTAMP-DLTM        TO OUT-TSTAMP-DLTM
           IF  INP-CAMPO4-DLTM         EQUAL ZEROS
               MOVE SPACES             TO OUT-CAMPO4-DLTM
           ELSE
               MOVE INP-CAMPO4-DLTM    TO OUT-CAMPO4-DLTM-N
           END-IF
           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
               MOVE INP-NOME-DLTM      (WRK-IND)
                                       TO OUT-NOME-DLTM   (WRK-IND)
               MOVE INP-RESTR-DLTM     (WRK-IND)
                                       TO OUT-RESTR-DLTM  (WRK-IND)
               MOVE INP-CNPJ-DLTM      (WRK-IND)
                                       TO OUT-CNPJ-DLTM   (WRK-IND)
               MOVE INP-CANC-DLTM      (WRK-IND)
                                       TO OUT-CANC-DLTM   (WRK-IND)
               MOVE INP-RATING-DLTM    (WRK-IND)
                                       TO OUT-RATING-DLTM (WRK-IND)
DTS001         MOVE WRK-INP-NIVEL-DLTM (WRK-IND)
DTS001                                 TO WRK-OUT-NIVEL-DLTM-X(WRK-IND)
               MOVE INP-CEP-DLTM       (WRK-IND)
                                       TO OUT-CEP-DLTM    (WRK-IND)
               MOVE INP-LIQSAC-DLTM    (WRK-IND)
                                       TO OUT-LIQSAC-DLTM (WRK-IND)
               MOVE INP-CEDSAC-DLTM    (WRK-IND)
                                       TO OUT-CEDSAC-DLTM (WRK-IND)
               MOVE INP-SACOPER-DLTM   (WRK-IND)
                                       TO OUT-SACOPER-DLTM(WRK-IND)
               MOVE INP-PERC1-DLTM     (WRK-IND)
                                       TO OUT-PERC1-DLTM  (WRK-IND)
               MOVE INP-PERC2-DLTM     (WRK-IND)
                                       TO OUT-PERC2-DLTM  (WRK-IND)
               MOVE INP-PERC3-DLTM     (WRK-IND)
                                       TO OUT-PERC3-DLTM  (WRK-IND)
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  COMU-AREA.

           IF  WRK-FLAG-ERRO         EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO
                                     TO COMU-MENSAGEM
           END-IF.

           MOVE LENGTH OF COMU-AREA  TO COMU-LL
           MOVE ZEROS                TO COMU-ZZ

           MOVE INP-FASE-DLTM        TO COMU-FASE
           MOVE INP-PRLIBER-DLTM     TO COMU-PRLIBER
           MOVE INP-VLLIBER-DLTM     TO COMU-VLLIBER
           MOVE INP-OPERAC-DLTM      TO COMU-OPERACAO
           MOVE INP-TRANSAC-DLTM     TO COMU-TRANSA-ANTE
           MOVE INP-FLAGENT-DLTM     TO COMU-FLAGENT
           MOVE INP-TSTAMP-DLTM      TO COMU-TMESTAMP
           MOVE INP-OPCAO-DLTM       TO COMU-OPCAO.

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE INP-CHAMADO-DLTM     TO WRK-TELA.
           MOVE 'DCOM0412'           TO COMU-TRANSACAO.
           MOVE INP-SENHAS-DLTM      TO COMU-SENHAS.
           MOVE COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0412'          TO WRK-TELA
                                       COMU-TRANSACAO.
           MOVE '5'                 TO COMU-PFK.
           MOVE COMU-AREA        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0412'         TO WRK-TELA
                                      COMU-TRANSACAO.
           MOVE 'A'                TO COMU-PFK.
           MOVE COMU-AREA       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1250-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF INP-PAGINA-DLTM  EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO OUT-MENSA-DLTM
              GO TO 1250-99-FIM
           END-IF.

           INITIALIZE 4230-ENTRADA-ROTEADOR
                      4230-SAIDA-ROTEADOR.

           COMPUTE 4230E-RESTART = (INP-PAGINA-DLTM - 2) * 4
           MOVE INP-OPERAC-DLTM        TO 4230E-OPERACAO

           PERFORM 1110-ACESSAR-DCOM4230.

           MOVE 'TECLE <PF3> PARA VOLTAR'
                                       TO OUT-MENSA-DLTM.

           COMPUTE OUT-PAGINA-DLTM = INP-PAGINA-DLTM - 1.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1260-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF INP-FIMAMOS-DLTM  EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO OUT-MENSA-DLTM
              GO TO 1260-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 4
               MOVE SPACES             TO OUT-OCOR-DLTM(WRK-IND)
           END-PERFORM.

           INITIALIZE 4230-ENTRADA-ROTEADOR
                      4230-SAIDA-ROTEADOR.

           COMPUTE 4230E-RESTART = (INP-PAGINA-DLTM) * 4
           MOVE INP-OPERAC-DLTM        TO 4230E-OPERACAO

           PERFORM 1110-ACESSAR-DCOM4230.

           MOVE 'TECLE <PF3> PARA VOLTAR'
                                       TO OUT-MENSA-DLTM.

           COMPUTE OUT-PAGINA-DLTM = INP-PAGINA-DLTM + 1.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

DTS001     MOVE LENGTH                 OF WRK-660-DCOMDLTM
DTS001                                 TO 660-LL-AREA-DLTM
DTS001     MOVE LENGTH                 OF OUTPUT-DLTM
DTS001                                 TO OUT-LL-DLTM
DTS001                                    660-LL-MENSAGEM-DLTM.

           CALL 'BRAD0660'             USING OUTPUT-DLTM
                                             WRK-660-DCOMDLTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE OUTPUT-DLTM            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0412'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0412'         TO ERR-PGM
           END-IF.


           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.


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


           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
