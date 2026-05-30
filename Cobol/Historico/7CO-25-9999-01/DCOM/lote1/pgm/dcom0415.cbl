      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0415.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0415                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   08/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   LIBERAR CONTRATO - CONTRATOS POR CLIENTE.   *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDOTM - LIBERAR CONTRATO - CONTRATOS POR CLIENTE.        *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSG - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    DCOM6416 - BUSCA NOME DO CLIENTE                            *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0413           CHAMA: DCOM0416            *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0415 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-193                     PIC S9(008) COMP    VALUE +193.
      *--- DISP     - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +225.
      *--- DISP     - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *--- DISP     - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49345                   PIC S9(008) COMP    VALUE +49345.
      *--- DISP     - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.
      *--- DISP     - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
      *--- DISP     - BRILHANTE - DESPROTEGIDO - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *--- NODISP   - NORMAL    -   PROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49381                   PIC S9(008) COMP    VALUE +49381.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-ERRO-ACESSO         PIC  X(01)          VALUE SPACES.
           05  WRK-IND                 PIC  9(002)         VALUE ZEROS.
           05  WRK-QTD-L               PIC  9(001)         VALUE ZEROS.
           05  WRK-QTD-D               PIC  9(001)         VALUE ZEROS.
           05  WRK-QTD-SPACES          PIC  9(001)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.
           05  WCT-SELEC               PIC S9(09) COMP     VALUE ZEROS.
           05  WRK-EDT-VAL             PIC ZZ.ZZZ.ZZZ.ZZ9,99.
           05  WRK-LIBEROU-OK          PIC X(01)           VALUE SPACES.

           05  WRK-MENSA-PF7           PIC  X(079) VALUE
               'PF7 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA VOLTAR'.

           05  WRK-MENSA-PF8           PIC  X(079) VALUE
               'PF8 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA AVANCAR'.

       01     WRK-CHAVE-4170.
         05   WRK-CONTRATO-4170        PIC  9(09)          VALUE ZEROS.
         05   WRK-VERSAO-4170          PIC  9(03)          VALUE ZEROS.
         05   WRK-FILLER               PIC  X(33)          VALUE SPACES.

       01     WRK-MENSAG-4170.
         05   WRK-FILLER-4170          PIC  X(13) VALUE 'BASE < DCOM >'.
         05   WRK-RESTO-4170           PIC  X(66) VALUE SPACES.

       01     WRK-AGENCIA-AUX.
         05   WRK-AGENCIA-NUM          PIC  9(05)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM6556 ***'.
      *----------------------------------------------------------------*
           COPY 'I#DCOM11'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4170 ***'.
      *----------------------------------------------------------------*

       01  4170-ENTRADA.
           03  4170-ENT-COD-RETORNO          PIC  X(04).
           03  4170-ENT-MSG-RETORNO          PIC  X(79).
           03  4170-ENT-RESTART              PIC  9(05).
           03  4170-ENT-FLAG                 PIC  X(01).
           03  4170-ENT-START-COUNT          PIC  X(01).
           03  4170-ENT-CFUNC-BDSCO          PIC  9(09).
           03  4170-ENT-CTERM                PIC  X(08).
           03  4170-ENT-CCHAVE-ELMTO-DESC    PIC  X(45).
           03  4170-ENT-CINDCD-PSSOA-DESC    PIC  X(02).
           03  4170-ENT-CELMTO-DESC-COML     PIC  9(03).

       01  4170-SAIDA.
           03  4170-SAI-COD-RET              PIC  X(04).
           03  4170-SAI-COD-RET-9     REDEFINES 4170-SAI-COD-RET
                                             PIC  9(04).
           03  4170-SAI-MSG-RET              PIC  X(79).
           03  4170-SAI-RESTART              PIC  9(05).
           03  4170-SAI-FLAG                 PIC  X(01).
           03  4170-SAI-QTDE-RETORNADA       PIC  9(03).
           03  4170-SAI-COUNT                PIC  9(09).
           03  4170-SAI-DADOS.
FS2511*        05  4170-SAI-CCNPJ-CPF        PIC  9(09).
FS2511         05  4170-SAI-CCNPJ-CPF        PIC  X(09).
FS2511*        05  4170-SAI-CFLIAL-CNPJ      PIC  9(05).
FS2511         05  4170-SAI-CFLIAL-CNPJ      PIC  X(04).
               05  4170-SAI-CCTRL-CNPJ-CPF   PIC  9(02).
               05  4170-SAI-CDOCTO-ID        PIC  X(15).
               05  4170-SAI-IPSSOA-DESC-COML PIC  X(60).
               05  4170-SAI-CCLUB            PIC  9(10).
               05  4170-SAI-CBCO             PIC  9(03).
               05  4170-SAI-NOME-BANCO       PIC  X(40).
               05  4170-SAI-CAG-BCRIA        PIC  9(05).
               05  4170-SAI-NOME-AGENCIA     PIC  X(40).
               05  4170-SAI-CCTA-BCRIA-CLI   PIC  9(13).
               05  4170-SAI-CPOSTO-SERVC     PIC  9(03).
               05  4170-SAI-IPSSOA-CONJG     PIC  X(40).
               05  4170-SAI-CCPF-CONJG       PIC  9(09).
               05  4170-SAI-CCTRL-CPF-CONJG  PIC  9(02).
               05  4170-SAI-CDOCTO-ID-CONJG  PIC  X(15).
               05  4170-SAI-CINDCD-CONJG-AUTRZ
                                             PIC  X(01).
               05  4170-SAI-CCEP             PIC  9(05).
               05  4170-SAI-CCEP-COMPL       PIC  9(03).
               05  4170-SAI-ELOGDR           PIC  X(40).
               05  4170-SAI-CSGL-UF          PIC  X(02).

           03  4170-SAI-CONSISTENCIA REDEFINES 4170-SAI-DADOS.
               05  4170-SAI-TAMANHO-ERRO     PIC  9(01).
               05  4170-SAI-TABELA-ERROS     OCCURS 1 TIMES.
                   07  4170-SAI-CAMPOS-ERRO  PIC  9(01).

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
           '*** AREA DA TELA DCOMDOTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DOTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DOTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DOTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DOTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DOTM.
               10  WRK-INP-FASE-DOTM-X.
                   15  WRK-INP-FASE-DOTM-N
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TIPOOPC-DOTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-INP-TMSTAMP-DOTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-FIMAMOS-DOTM PIC  X(01)          VALUE SPACE.
               10  WRK-INP-PAGINA-DOTM-X.
                   15  WRK-INP-PAGINA-DOTM-N
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-NMCLIE-DOTM  PIC  X(040) VALUE SPACES.
               10  WRK-INP-CNPJX-DOTM   PIC  X(09)  VALUE SPACES.
               10  WRK-INP-CNPJCPF-DOTM.
FS2511*            15  WRK-INP-NUMERO-DOTM
FS2511*                                 PIC  999.999.999.
FS2511             15  WRK-INP-NUMERO-DOTM PIC  X(11) VALUE SPACES.
FS2511*            15  WRK-INP-NUMERO-DOTM
FS2511*                                 PIC  999.999.999.
FS2511                                      PIC  X(04)  VALUE SPACES.
FS2511*            15  WRK-INP-FILIAL-DOTM
FS2511*                                     PIC  9(04)  VALUE ZEROS.
FS2511             15  WRK-INP-FILIAL-DOTM
FS2511                                      PIC  X(04)  VALUE SPACES.
                   15  WRK-INP-HIFEN-2-DOTM
                                        PIC  X(01)  VALUE SPACES.
                   15  WRK-INP-CONTROLE-DOTM
                                        PIC  9(02)  VALUE ZEROS.
               10  WRK-INP-BANCOX-DOTM  PIC  X(06)  VALUE SPACES.
               10  WRK-INP-BANCO-DOTM-X.
                   15  WRK-INP-BANCO-DOTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-AGENX-DOTM   PIC  X(08)  VALUE SPACES.
               10  WRK-INP-AGENCIA-GR-DOTM.
                   15  WRK-INP-CODAGE-DOTM
                                        PIC  9(005) VALUE ZEROS.
                   15  WRK-INP-HIFEN-AG-DOTM
                                        PIC  X(001) VALUE SPACES.
                   15  WRK-INP-DESCAGEN-DOTM
                                        PIC  X(023) VALUE SPACES.
               10  WRK-INP-CONTAX-DOTM  PIC  X(09)  VALUE SPACES.
               10  WRK-INP-CONTA-DOTM-X.
                   15  WRK-INP-CONTA-DOTM
                                        PIC  9(013) VALUE ZEROS.
               10  WRK-INP-TIPO-PESQ-DOTM PIC  9(001) VALUE ZEROS.
               10  WRK-INP-TABELA-DOTM    OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DOTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-CONTR-DOTM-X.
                       20  WRK-INP-CONTR-DOTM PIC  9(009) VALUE ZEROS.
                   15  WRK-INP-NOME-DOTM      PIC  X(030) VALUE SPACES.
                   15  WRK-INP-DTINIC-DOTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTVENCT-DOTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-LIMITE-DOTM    PIC  X(017) VALUE SPACES.
                   15  WRK-INP-SITUAC-DOTM    PIC  X(020) VALUE SPACES.
           05  WRK-INP-MENSA-DOTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDOTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DOTM.
           05  WRK-OUT-LL-DOTM          PIC S9(004) COMP VALUE +0852.
           05  WRK-OUT-ZZ-DOTM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DOTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-DOTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DOTM.
               10  WRK-OUT-FASE-DOTM-X.
                   15  WRK-OUT-FASE-DOTM-N
                                        PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TIPOOPC-DOTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-TMSTAMP-DOTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-FIMAMOS-DOTM PIC  X(01)          VALUE SPACE.
               10  WRK-OUT-PAGINA-DOTM-X.
                   15  WRK-OUT-PAGINA-DOTM-N
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-NMCLIE-DOTM  PIC  X(040) VALUE SPACES.
               10  WRK-OUT-CNPJX-DOTM   PIC  X(09)  VALUE SPACES.
               10  WRK-OUT-CNPJCPF-DOTM.
FS2511*            15  WRK-OUT-NUMERO-DOTM
FS2511*                                 PIC  999.999.999.
FS2511             15  WRK-OUT-NUMERO-DOTM
FS2511                                  PIC  X(11) VALUE SPACES.
                   15  WRK-OUT-BARRA-1-DOTM
                                        PIC  X(001) VALUE SPACES.
FS2511*            15  WRK-OUT-FILIAL-DOTM
FS2511*                                 PIC  9(004) VALUE ZEROS.
FS2511             15  WRK-OUT-FILIAL-DOTM
FS2511                                  PIC  X(004) VALUE SPACES.
                   15  WRK-OUT-HIFEN-2-DOTM
                                        PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-CONTROLE-DOTM
                                        PIC  9(002) VALUE ZEROS.
               10  WRK-OUT-BANCOX-DOTM  PIC  X(06)  VALUE SPACES.
               10  WRK-OUT-BANCO-DOTM-X.
                   15  WRK-OUT-BANCO-DOTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-AGENX-DOTM   PIC  X(08)  VALUE SPACES.
               10  WRK-OUT-AGENCIA-GR-DOTM.
                   15  WRK-OUT-CODAGE-DOTM-X.
                       20  WRK-OUT-CODAGE-DOTM
                                        PIC  9(005) VALUE ZEROS.
                   15  WRK-OUT-HIFEN-AG-DOTM
                                        PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-DESCAGEN-DOTM
                                        PIC  X(023) VALUE SPACES.
               10  WRK-OUT-CONTAX-DOTM  PIC  X(09)  VALUE SPACES.
               10  WRK-OUT-CONTA-DOTM-X.
                   15  WRK-OUT-CONTA-DOTM
                                        PIC  9(013) VALUE ZEROS.
               10  WRK-OUT-TIPO-PESQ-DOTM PIC  9(001) VALUE ZEROS.
               10  WRK-OUT-TABELA-DOTM.
                   15  WRK-OUT-LINHAS-DOTM OCCURS 05 TIMES.
                       20  WRK-OUT-SELEC-ATTR-DOTM
                                       PIC S9(004) COMP VALUE ZEROS.
                       20  WRK-OUT-SELEC-DOTM
                                               PIC  X(001) VALUE SPACES.
                       20  WRK-OUT-CONTR-DOTM-X.
                           25  WRK-OUT-CONTR-DOTM
                                               PIC  9(009) VALUE ZEROS.
                       20  WRK-OUT-NOME-DOTM
                                               PIC  X(030) VALUE SPACES.
                       20  WRK-OUT-DTINIC-DOTM
                                               PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-DTVENCT-DOTM
                                               PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-LIMITE-DOTM-X.
                           25  WRK-OUT-LIMITE-DOTM
                                               PIC  X(017) VALUE SPACES.
                       20  WRK-OUT-SITUAC-DOTM
                                               PIC  X(020) VALUE SPACES.
               10  WRK-OUT-MENSA-DOTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDOTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDOTM.
           05  660-LL-AREA-DOTM        PIC  9(04)  COMP  VALUE 0110.
           05  660-LL-MENSAGEM-DOTM    PIC  9(04)  COMP  VALUE 0852.
           05  660-SENHAS-DOTM         PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-DOTM        PIC  9(04)  COMP  VALUE 0068.
           05  660-FASE-DOTM           PIC  9(04)  COMP  VALUE 0001.
           05  660-TIPOOPC-DOTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-TMSTAMP-DOTM        PIC  9(04)  COMP  VALUE 0026.
           05  660-FIMAMOS-DOTM        PIC  9(04)  COMP  VALUE 0001.
           05  660-PAGINA-DOTM         PIC  9(04)  COMP  VALUE 0003.
           05  660-NMCLIE-DOTM         PIC  9(04)  COMP  VALUE 0040.
           05  660-CNPJX-DOTM          PIC  9(04)  COMP  VALUE 0009.
           05  660-CNPJCPF-DOTM        PIC  9(04)  COMP  VALUE 0019.
           05  660-BANCOX-DOTM         PIC  9(04)  COMP  VALUE 0006.
           05  660-BANCO-DOTM          PIC  9(04)  COMP  VALUE 0003.
           05  660-AGENX-DOTM          PIC  9(04)  COMP  VALUE 0008.
           05  660-AGENCIA-DOTM        PIC  9(04)  COMP  VALUE 0029.
           05  660-CONTAX-DOTM         PIC  9(04)  COMP  VALUE 0009.
           05  660-CONTA-DOTM          PIC  9(04)  COMP  VALUE 0013.
           05  660-TPPESQ-DOTM         PIC  9(04)  COMP  VALUE 0001.
           05  660-TABELA-OCCURS       OCCURS   5  TIMES.
               10  660-SELEC-DOTM      PIC  9(04)  COMP  VALUE 2003.
               10  660-CONTR-DOTM      PIC  9(04)  COMP  VALUE 0009.
               10  660-NOME-DOTM       PIC  9(04)  COMP  VALUE 0030.
               10  660-DTINIC-DOTM     PIC  9(04)  COMP  VALUE 0010.
               10  660-DTVENCT-DOTM    PIC  9(04)  COMP  VALUE 0010.
               10  660-LIMITE-DOTM     PIC  9(04)  COMP  VALUE 0017.
               10  660-SITUAC-DOTM     PIC  9(04)  COMP  VALUE 0020.
           05  660-MENSA-DOTM          PIC  9(04)  COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0413 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-413.
           05  COMU-LL-413            PIC S9(004) COMP    VALUE +193.
           05  COMU-ZZ-413            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-413.
               10  COMU-TRANSACAO-413 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-413       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-413        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-413       PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-413.
               10  COMU-OPCAO-413    PIC  9(001)          VALUE ZEROS.
               10  COMU-TMSTAMP-413  PIC  X(026)          VALUE SPACES.
               10  COMU-CODAGE-413   PIC  9(005)          VALUE ZEROS.
               10  COMU-CONTA-413    PIC  9(013)          VALUE ZEROS.
FS2511*        10  COMU-NUMERO-413   PIC  9(009)          VALUE ZEROS.
FS2511         10  COMU-NUMERO-413   PIC  X(009)          VALUE SPACES.
FS2511*        10  COMU-FILIAL-413   PIC  9(004)          VALUE ZEROS.
FS2511         10  COMU-FILIAL-413   PIC  X(04)           VALUE SPACES.
               10  COMU-CONTROLE-413 PIC  9(002)          VALUE ZEROS.

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP  VALUE +230.
           05  COMU-ZZ                 PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  COMU-PFK            PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPCAO          PIC  9(001)       VALUE ZEROS.
               10  COMU-TIMESTAMP      PIC  X(026)       VALUE SPACES.
               10  COMU-CONTRATO       PIC  9(009)       VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)       VALUE ZEROS.
               10  COMU-CPF-X.
FS2511*            15  COMU-CPF        PIC  9(009)       VALUE ZEROS.
FS2511             15  COMU-CPF        PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL         PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL         PIC  X(04)        VALUE SPACES.
               10  COMU-DIGITO         PIC  9(002)       VALUE ZEROS.
               10  COMU-MENSAG         PIC  X(079)       VALUE SPACES.

      *---------------------------------------------------------------
      *        *** AREA DE COMUNICACAO CHAMANDO DCOM0416
      *---------------------------------------------------------------

       01  COMU-AREA-416.
           05  COMU-LL-416            PIC S9(004) COMP    VALUE +276.
           05  COMU-ZZ-416            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-416.
               10  COMU-TRANSACAO-416 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-416        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-416       PIC  X(068)         VALUE SPACES.
           05  COMU-OPCAO-416         PIC  9(001)         VALUE ZEROS.
           05  COMU-TMSTAMP-416       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO-416      PIC  9(009)         VALUE ZEROS.
           05  COMU-CODAGE-416-X.
               10  COMU-CODAGE-416-N  PIC  9(005)         VALUE ZEROS.
           05  COMU-CONTA-416-X.
               10  COMU-CONTA-416-N   PIC  9(013)          VALUE ZEROS.
           05  COMU-NUMERO-416-X.
FS2511*        10  COMU-NUMERO-416-N  PIC  9(009)          VALUE ZEROS.
FS2511         10  COMU-NUMERO-416-N  PIC  X(009)          VALUE SPACES.
           05  COMU-FILIAL-416-X.
FS2511*        10  COMU-FILIAL-416-N  PIC  9(004)          VALUE ZEROS.
FS2511         10  COMU-FILIAL-416-N  PIC  X(04)           VALUE SPACES.
           05  COMU-CONTROLE-416-X.
               10  COMU-CONTROLE-416-N PIC 9(002)          VALUE ZEROS.
           05  COMU-PAGINA-416         PIC 9(004)          VALUE ZEROS.
           05  COMU-MENSA-416          PIC X(079)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM5142 **'.
      *----------------------------------------------------------------*

       01  WRK-5142E-ENTRADA.
           03  WRK-5142E-COD-RETORNO      PIC  X(004).
           03  WRK-5142E-MSG-RETORNO      PIC  X(079).
           03  WRK-5142E-RESTART          PIC  9(005).
           03  WRK-5142E-FLAG             PIC  X(001).
           03  WRK-5142E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-5142E-CTERM            PIC  X(008).
           03  WRK-5142E-CCONTR-LIM-DESC  PIC  9(009).

       01  WRK-5142S-SAIDA.
           03  WRK-5142S-HEADER.
               05  WRK-5142S-COD-RETORNO      PIC  X(004).
               05  WRK-5142S-MSG-RETORNO      PIC  X(079).
               05  WRK-5142S-RESTART          PIC  9(005).
               05  WRK-5142S-FLAG             PIC  X(001).
               05  WRK-5142S-COUNT            PIC  9(009).
               05  WRK-5142S-TAMANHO-ERRO     PIC  9(001).
               05  WRK-5142S-CAMPOS-ERRO      PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4112 **'.
      *----------------------------------------------------------------*
       01  WRK-4112E-ENTRADA.
           03  WRK-4112E-HEADER.
               05  WRK-4112E-COD-RETORNO             PIC  X(04).
               05  WRK-4112E-MSG-RETORNO             PIC  X(79).
               05  WRK-4112E-RESTART                 PIC  9(05).
               05  WRK-4112E-FLAG                    PIC  X(01).
               05  WRK-4112E-START-COUNT             PIC  X(01).
               05  WRK-4112E-CFUNC-BDSCO             PIC  9(09).
               05  WRK-4112E-CTERM                   PIC  X(08).
           03  WRK-4112E-DADOS.
FS2411*        05  WRK-4112E-CCNPJ-CPF               PIC  9(09).
FS2411         05  WRK-4112E-CCNPJ-CPF               PIC  X(09).
FS2511*        05  WRK-4112E-CFLIAL-CNPJ             PIC  9(04).
FS2511         05  WRK-4112E-CFLIAL-CNPJ             PIC  X(04).
               05  WRK-4112E-CCTRL-CNPJ-CPF          PIC  9(02).
               05  WRK-4112E-FLAG-FILTRO             PIC  X(01).

       01  WRK-4112S-SAIDA.
           03  WRK-4112S-HEADER.
               05  WRK-4112S-COD-RETORNO            PIC  X(04).
               05  WRK-4112S-MSG-RETORNO            PIC  X(79).
               05  WRK-4112S-RESTART                PIC  9(05).
               05  WRK-4112S-FLAG                   PIC  X(01).
               05  WRK-4112S-QTDE-RETORNADA         PIC  9(03).
               05  WRK-4112S-COUNT                  PIC  9(09).
           03  WRK-4112S-DADOS.
               05  WRK-4112S-TABELA-SAIDA    OCCURS 30 TIMES.
                   10  WRK-4112S-NM-CLIE            PIC  X(25).
                   10  WRK-4112S-CCONTR-LIM-DESC    PIC  9(09).
                   10  WRK-4112S-CVRSAO-CONTR-LIM   PIC  9(03).
                   10  WRK-4112S-CAG-BCRIA          PIC  9(05).
                   10  WRK-4112S-CCTA-BCRIA         PIC  9(13).
                   10  WRK-4112S-DINIC-VGCIA-CONTR  PIC  X(10).
                   10  WRK-4112S-DVCTO-CONTR-LIM    PIC  X(10).
                   10  WRK-4112S-VCONTR-LIM         PIC  9(15)V99.
                   10  WRK-4112S-IRSUMO-SIT-DESC    PIC  X(15).
           03  WRK-4112S-CONSISTENCIA   REDEFINES   WRK-4112S-DADOS.
               05  WRK-4112S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4112S-TABELA-ERRO   OCCURS   1    TIMES.
                   10  WRK-4112S-CAMPOS-ERRO        PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM6416 **'.
      *----------------------------------------------------------------*

       01  WRK-6416E-ENTRADA.
           03  WRK-6416E-COD-RETORNO      PIC  X(004).
           03  WRK-6416E-MSG-RETORNO      PIC  X(079).
           03  WRK-6416E-RESTART          PIC  9(005).
           03  WRK-6416E-FLAG             PIC  X(001).
           03  WRK-6416E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-6416E-CTERM            PIC  X(008).
           03  WRK-6416E-BANCO            PIC  9(003).
           03  WRK-6416E-DEPDC            PIC  9(005).

       01  WRK-6416S-SAIDA.
           03  WRK-6416S-HEADER.
               05  WRK-6416S-COD-RETORNO  PIC  X(004).
               05  WRK-6416S-MSG-RETORNO  PIC  X(079).
               05  WRK-6416S-RESTART      PIC  9(005).
               05  WRK-6416S-FLAG         PIC  X(001).
           03  WRK-6416S-DADOS.
               05  WRK-6416S-TABELA-SAIDA.
                   07  WRK-6416S-AG-DEPDC            PIC 9(05).
                   07  WRK-6416S-DG-DEPDC            PIC X(01).
                   07  WRK-6416S-NOME-DEPDC          PIC X(40).
                   07  WRK-6416S-END-DEPDC           PIC X(30).
                   07  WRK-6416S-DIRETORIA-REG       PIC 9(05).
                   07  WRK-6416S-DIRETORIA-NOME      PIC X(40).
                   07  WRK-6416S-NOME-BANCO          PIC X(40).
                   07  WRK-6416S-POLO-SERVICO        PIC 9(05).
                   07  WRK-6416S-MUNIC-DEPDC         PIC X(25).
                   07  WRK-6416S-UF                  PIC X(02).
                   07  WRK-6416S-CEP                 PIC 9(05).
                   07  WRK-6416S-CEP-COMPL           PIC 9(03).
                   07  WRK-6416S-INDIC-DEPDC-AT      PIC X(01).
                   07  WRK-6416S-NATUREZA-DEPDC      PIC X(01).
                   07  WRK-6416S-COD-DISTR-CORR      PIC 9(05).
                   07  WRK-6416S-DENCRR-DEPDC-BDSCO  PIC X(10).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4111 **'.
      *----------------------------------------------------------------*
       01  WRK-4111E-ENTRADA.
           03  WRK-4111E-HEADER.
               05  WRK-4111E-COD-RETORNO             PIC  X(04).
               05  WRK-4111E-MSG-RETORNO             PIC  X(79).
               05  WRK-4111E-RESTART                 PIC  9(05).
               05  WRK-4111E-FLAG                    PIC  X(01).
               05  WRK-4111E-START-COUNT             PIC  X(01).
               05  WRK-4111E-CFUNC-BDSCO             PIC  9(09).
               05  WRK-4111E-CTERM                   PIC  X(08).
           03  WRK-4111E-DADOS.
               05  WRK-4111E-CAG-BCRIA               PIC  9(05).
               05  WRK-4111E-CCTA-BCRIA              PIC  9(13).
               05  WRK-4111E-FLAG-FILTRO             PIC  X(01).

       01  WRK-4111S-SAIDA.
           03  WRK-4111S-HEADER.
               05  WRK-4111S-COD-RETORNO            PIC  X(04).
               05  WRK-4111S-MSG-RETORNO            PIC  X(79).
               05  WRK-4111S-RESTART                PIC  9(05).
               05  WRK-4111S-FLAG                   PIC  X(01).
               05  WRK-4111S-QTDE-RETORNADA         PIC  9(03).
               05  WRK-4111S-COUNT                  PIC  9(09).
           03  WRK-4111S-DADOS.
               05  WRK-4111S-TABELA-SAIDA    OCCURS 20 TIMES.
                   10  WRK-4111S-CCONTR-LIM-DESC    PIC  9(09).
                   10  WRK-4111S-VERSAO-CCONTR-LIM  PIC  9(03).
FS2511*            10  WRK-4111S-CCNPJ-CPF          PIC  9(09).
FS2511             10  WRK-4111S-CCNPJ-CPF          PIC  X(09).
FS2511*            10  WRK-4111S-CFLIAL-CNPJ        PIC  9(04).
FS2511             10  WRK-4111S-CFLIAL-CNPJ        PIC  X(04).
                   10  WRK-4111S-CCTRL-CNPJ-CPF     PIC  9(02).
                   10  WRK-4111S-NM-CLIE            PIC  X(25).
                   10  WRK-4111S-DINIC-VGCIA-CONTR  PIC  X(10).
                   10  WRK-4111S-DVCTO-CONTR-LIM    PIC  X(10).
                   10  WRK-4111S-VCONTR-LIM         PIC  9(15)V99.
                   10  WRK-4111S-IRSUMO-SIT-DESC    PIC  X(15).
           03  WRK-4111S-CONSISTENCIA   REDEFINES   WRK-4111S-DADOS.
               05  WRK-4111S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4111S-TABELA-ERRO   OCCURS   2    TIMES.
                   10  WRK-4111S-CAMPOS-ERRO        PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
           COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM6408 ***'.
      *----------------------------------------------------------------*
           COPY  'I#DCOMX3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM11   ***'.
      *----------------------------------------------------------------*
           COPY  'I#DCOM11'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
           COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD0431  ***'.
      *----------------------------------------------------------------*
       01  AREA-BRAD0431.
           03  WRK-CONTA-0431          PIC  9(13) COMP-3.
           03  WRK-CONTA-0431-R        PIC  9(13)          VALUE ZEROS.
           03  WRK-DIGITO-0431         PIC  X(01)          VALUE SPACES.
           03  WRK-DIGITO-0431-R REDEFINES WRK-DIGITO-0431 PIC 9(01).
           03  WRK-TAMANHO-0431        PIC  9(02)          VALUE 7.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0415 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO  WRK-OUT-DADOS-DOTM
                                           WRK-INP-DADOS-DOTM
                                           WRK-OUT-SENHAS-DOTM
                                           WRK-OUT-COMANDO-DOTM
                                           WRK-INCONSIS.

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

           MOVE 'DCOMDOTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    EQUAL  'DCOM0413'
               PERFORM 1100-RECEBER-CONTROLE-413
               MOVE  1             TO WRK-INP-FASE-DOTM-N
                                      WRK-OUT-FASE-DOTM-N
               MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "L" E T
      -        'ECLE <ENTER>'      TO  WRK-OUT-MENSA-DOTM
           ELSE
             IF  WRK-TRANSACAO    EQUAL  'DCOM0416'
                 MOVE 'S'            TO WRK-LIBEROU-OK
                 PERFORM 1200-RECEBER-CONTROLE-416
                 MOVE  1             TO WRK-INP-FASE-DOTM-N
                                        WRK-OUT-FASE-DOTM-N
                 MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "L" E
      -          ' TECLE <ENTER>'    TO  WRK-OUT-MENSA-DOTM

                 IF   COMU-MENSA-416         NOT EQUAL SPACES
                      MOVE COMU-MENSA-416    TO  WRK-OUT-MENSA-DOTM
                 END-IF

             ELSE
                 PERFORM 1300-PROCESSAR-DCOMDOTM
             END-IF
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDOTM'
               PERFORM 1700-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DO DCOM0413                     *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE-413       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  COMU-AREA-413.
           MOVE ZEROS                  TO  WRK-INP-PAGINA-DOTM-N
                                           WRK-OUT-PAGINA-DOTM-N.

           MOVE COMU-TMSTAMP-413       TO  WRK-INP-TMSTAMP-DOTM
                                           WRK-OUT-TMSTAMP-DOTM
           MOVE COMU-OPCAO-413         TO  WRK-INP-TIPOOPC-DOTM
                                           WRK-OUT-TIPOOPC-DOTM
           MOVE ZEROS                  TO  WRK-4111E-RESTART
                                           WRK-4112E-RESTART
           MOVE 1                      TO  WRK-OUT-PAGINA-DOTM-N


           IF COMU-CODAGE-413  IS NUMERIC AND
              COMU-CODAGE-413  IS GREATER ZEROS

              MOVE  1                  TO  WRK-INP-TIPO-PESQ-DOTM
                                           WRK-OUT-TIPO-PESQ-DOTM
              MOVE 237                 TO  WRK-INP-BANCO-DOTM
                                           WRK-OUT-BANCO-DOTM
              MOVE COMU-CODAGE-413     TO  WRK-INP-CODAGE-DOTM
                                           WRK-OUT-CODAGE-DOTM
                                           WRK-4111E-CAG-BCRIA
              MOVE '-'                 TO  WRK-INP-HIFEN-AG-DOTM
                                           WRK-OUT-HIFEN-AG-DOTM
              MOVE COMU-CONTA-413      TO  WRK-INP-CONTA-DOTM
                                           WRK-OUT-CONTA-DOTM
                                           WRK-4111E-CCTA-BCRIA

              PERFORM  1110-MONTAR-CABECALHO

              MOVE 1                   TO  WRK-INP-PAGINA-DOTM-N
              PERFORM 1500-ACESSAR-DCOM4111

           ELSE

              MOVE  2                  TO  WRK-INP-TIPO-PESQ-DOTM
                                           WRK-OUT-TIPO-PESQ-DOTM
              MOVE COMU-NUMERO-413     TO  WRK-INP-NUMERO-DOTM
                                           WRK-OUT-NUMERO-DOTM
                                           WRK-4112E-CCNPJ-CPF
              MOVE '/'                 TO  WRK-INP-BARRA-1-DOTM
                                           WRK-OUT-BARRA-1-DOTM
              MOVE COMU-FILIAL-413     TO  WRK-INP-FILIAL-DOTM
                                           WRK-OUT-FILIAL-DOTM
                                           WRK-4112E-CFLIAL-CNPJ
              MOVE '-'                 TO  WRK-INP-HIFEN-2-DOTM
                                           WRK-OUT-HIFEN-2-DOTM
              MOVE COMU-CONTROLE-413   TO  WRK-INP-CONTROLE-DOTM
                                           WRK-OUT-CONTROLE-DOTM
                                           WRK-4112E-CCTRL-CNPJ-CPF

              PERFORM  1110-MONTAR-CABECALHO

              PERFORM 1600-ACESSAR-DCOM4112
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA MONTA O CABECALHO POR AGENCIA E CONTA             *
      *----------------------------------------------------------------*
       1110-MONTAR-CABECALHO           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-TIPO-PESQ-DOTM    EQUAL  1
               PERFORM  1120-ACESSAR-DCOM6416
               PERFORM  1130-CALCULAR-DIGITO
           END-IF.

           PERFORM 1140-ACESSAR-DCOM6408.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO DCOM6416                          *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM6416           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-6416E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-6416E-CTERM
           MOVE 237                    TO  WRK-6416E-BANCO
           MOVE WRK-INP-CODAGE-DOTM    TO  WRK-6416E-DEPDC
           MOVE 'DCOM6416'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-6416E-ENTRADA
                                       WRK-6416S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                    MOVE WRK-6416S-NOME-DEPDC   TO WRK-INP-DESCAGEN-DOTM
                                                   WRK-OUT-DESCAGEN-DOTM

               WHEN '0003'
                    MOVE  ALL '*'               TO WRK-INP-DESCAGEN-DOTM
                                                   WRK-OUT-DESCAGEN-DOTM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  WRK-6416S-COD-RETORNO
                                        TO WRK-OUT-MENSA-DOTM(1:4)
                    MOVE  '-'           TO WRK-OUT-MENSA-DOTM(6:1)
                    MOVE  WRK-6416S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-DOTM(8:72)
           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-CALCULAR-DIGITO            SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0431'   USING WRK-CONTA-0431
                                   WRK-DIGITO-0431
                                   WRK-TAMANHO-0431.


           IF  WRK-DIGITO-0431  EQUAL  '.'
               MOVE 'APL'          TO ERR-TIPO-ACESSO
               MOVE 'ERRO NO CALCULO DO DIGITO - BRAD0431 - CONTA = '
                                   TO ERR-TEXTO(1:47)
               MOVE WRK-CONTA-0431      TO WRK-CONTA-0431-R
               MOVE WRK-CONTA-0431-R    TO ERR-TEXTO(48:13)
                   MOVE ' 1130-CALCULAR-DIGITO -ERRO'
                                       TO WRK-OUT-MENSA-DOTM
           ELSE
               IF  WRK-DIGITO-0431       EQUAL  'P'
                   MOVE ZEROS       TO WRK-DIGITO-0431-R
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                    BUSCA NOME DO CLIENTE
      *----------------------------------------------------------------*
       1140-ACESSAR-DCOM6408           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE    ROTENT-6408-ENVIO.

           MOVE   ZEROS                TO  ROTENT-6408-COD-ENV
                                           ROTENT-6408-REST-ENV

           MOVE   SPACES               TO  ROTENT-6408-MSG-ENV
           MOVE   'N'                  TO  ROTENT-6408-FLAG-ENV
                                           ROTENT-6408-START-ENV

           MOVE  WRK-COD-USER          TO ROTENT-6408-CFUNC-ENV
           MOVE  LNK-IO-LTERM          TO ROTENT-6408-CTERM-ENV

           IF  WRK-INP-CODAGE-DOTM      IS  NUMERIC   AND
               WRK-INP-CODAGE-DOTM      GREATER  ZERO
               MOVE  WRK-INP-CODAGE-DOTM TO ROTENT-6408-CJUNC-ENV
               MOVE  WRK-INP-CONTA-DOTM  TO ROTENT-6408-CCTAC-ENV
               MOVE  WRK-DIGITO-0431-R   TO ROTENT-6408-DCTAC-ENV
FS2511         MOVE SPACES               TO ROTENT-6408-NUM-CNPJ-ENV
                                            ROTENT-6408-FIL-CNPJ-ENV
FS2511         MOVE ZEROS                TO ROTENT-6408-CTR-CNPJ-ENV
               MOVE 'BANCO:'             TO WRK-INP-BANCOX-DOTM
                                            WRK-OUT-BANCOX-DOTM
               MOVE 'AGENCIA:'           TO WRK-INP-AGENX-DOTM
                                            WRK-OUT-AGENX-DOTM
               MOVE 'CONTA...:'          TO WRK-INP-CONTAX-DOTM
                                            WRK-OUT-CONTAX-DOTM
           ELSE
               MOVE WRK-INP-NUMERO-DOTM
                                       TO ROTENT-6408-NUM-CNPJ-ENV
               MOVE WRK-INP-FILIAL-DOTM
                                       TO ROTENT-6408-FIL-CNPJ-ENV
               MOVE WRK-INP-CONTROLE-DOTM
                                       TO ROTENT-6408-CTR-CNPJ-ENV
               MOVE ZEROS              TO ROTENT-6408-CJUNC-ENV
                                          ROTENT-6408-CCTAC-ENV
                                          ROTENT-6408-DCTAC-ENV
               MOVE 'CNPJ/CPF:'        TO WRK-INP-CNPJX-DOTM
                                          WRK-OUT-CNPJX-DOTM
           END-IF.

           MOVE  'DCOM6408'            TO WRK-MODULO.

           CALL WRK-MODULO  USING ROTENT-6408-AREA
                                  ROTSAI-6408-RETORNO
                                  ERRO-AREA
                                  WRK-SQLCA.

           EVALUATE ROTSAI-6408-COD-RET

               WHEN '0000'

                   MOVE ROTSAI-6408-NM-RZSOC-RET(1)
                                       TO WRK-OUT-NMCLIE-DOTM
                                          WRK-INP-NMCLIE-DOTM

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   MOVE '0070'         TO ERR-LOCAL
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   INITIALIZE  COMU-AREA
                   MOVE ROTSAI-6408-MENS-RET TO COMU-MENSAG
                   PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
           END-EVALUATE.

      *----------------------------------------------------------------*
       1140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-RETORNA-CHAMADOR-DCOM0413  SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA     TO COMU-LL
           MOVE WRK-CHNG             TO WRK-FUNCAO
           MOVE 'DCOM0413'           TO WRK-TELA
           MOVE 'DCOM0415'           TO COMU-TRANSACAO
           MOVE WRK-INP-TIPOOPC-DOTM TO COMU-OPCAO
           MOVE WRK-INP-TMSTAMP-DOTM TO COMU-TIMESTAMP
           MOVE COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1150-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DO DCOM0416                     *
      *----------------------------------------------------------------*
       1200-RECEBER-CONTROLE-416       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  COMU-AREA-416.

           MOVE COMU-TMSTAMP-416       TO  WRK-INP-TMSTAMP-DOTM
                                           WRK-OUT-TMSTAMP-DOTM
           MOVE COMU-OPCAO-416         TO  WRK-INP-TIPOOPC-DOTM
                                           WRK-OUT-TIPOOPC-DOTM
           MOVE ZEROS                  TO  WRK-4111E-RESTART
                                           WRK-4112E-RESTART.
           MOVE 1                      TO  WRK-INP-PAGINA-DOTM-N
                                           WRK-OUT-PAGINA-DOTM-N

           IF COMU-CODAGE-416-X  IS NUMERIC AND
              COMU-CODAGE-416-N  IS GREATER ZEROS
              MOVE  1                  TO  WRK-INP-TIPO-PESQ-DOTM
                                           WRK-OUT-TIPO-PESQ-DOTM
              MOVE 237                 TO  WRK-INP-BANCO-DOTM
                                           WRK-OUT-BANCO-DOTM
              MOVE COMU-CODAGE-416-N   TO  WRK-INP-CODAGE-DOTM
                                           WRK-OUT-CODAGE-DOTM
                                           WRK-4111E-CAG-BCRIA
              MOVE '-'                 TO  WRK-INP-HIFEN-AG-DOTM
                                           WRK-OUT-HIFEN-AG-DOTM
              MOVE COMU-CONTA-416-N    TO  WRK-INP-CONTA-DOTM
                                           WRK-OUT-CONTA-DOTM
                                           WRK-4111E-CCTA-BCRIA

              PERFORM  1110-MONTAR-CABECALHO

              PERFORM 1500-ACESSAR-DCOM4111

           ELSE

              MOVE  2                  TO  WRK-INP-TIPO-PESQ-DOTM
                                           WRK-OUT-TIPO-PESQ-DOTM
              MOVE COMU-NUMERO-416-N   TO  WRK-INP-NUMERO-DOTM
                                           WRK-OUT-NUMERO-DOTM
                                           WRK-4112E-CCNPJ-CPF
              MOVE '/'                 TO  WRK-INP-BARRA-1-DOTM
                                           WRK-OUT-BARRA-1-DOTM
              MOVE COMU-FILIAL-416-N   TO  WRK-INP-FILIAL-DOTM
                                           WRK-OUT-FILIAL-DOTM
                                           WRK-4112E-CFLIAL-CNPJ
              MOVE '-'                 TO  WRK-INP-HIFEN-2-DOTM
                                           WRK-OUT-HIFEN-2-DOTM
              MOVE COMU-CONTROLE-416-N TO  WRK-INP-CONTROLE-DOTM
                                           WRK-OUT-CONTROLE-DOTM
                                           WRK-4112E-CCTRL-CNPJ-CPF

              PERFORM  1110-MONTAR-CABECALHO

              PERFORM 1600-ACESSAR-DCOM4112
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDOTM        *
      *----------------------------------------------------------------*
       1300-PROCESSAR-DCOMDOTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DOTM.
           PERFORM 1310-DEVOLVER-TELA-FIXA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DOTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DOTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DOTM EQUAL 'PFK1')
                    PERFORM 1311-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-DOTM TO WRK-OUT-MENSA-DOTM

               WHEN WRK-INP-PFK-DOTM     EQUAL '3' OR
                   (WRK-INP-PFK-DOTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DOTM EQUAL 'PFK3')
                    PERFORM 1320-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DOTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DOTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DOTM EQUAL 'PFK5')
                    PERFORM 1330-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DOTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DOTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DOTM EQUAL 'PFK10')
                    PERFORM 1340-RETORNAR-MENU-DCOM

               WHEN WRK-INP-FASE-DOTM-N  EQUAL 1
                    PERFORM 1350-TRATAR-FASE-1

               WHEN WRK-INP-FASE-DOTM-N  EQUAL 2
                    PERFORM 1360-TRATAR-FASE-2

               WHEN OTHER
                    PERFORM 1311-DEVOLVER-TELA-VARIA
                    MOVE SG-MENSAGEM (0038)
                                         TO WRK-OUT-MENSA-DOTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS FIXOS DA TELA SEM ALTERACAO      *
      *----------------------------------------------------------------*
       1310-DEVOLVER-TELA-FIXA         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-PAGINA-DOTM-X   TO WRK-OUT-PAGINA-DOTM-X.
           MOVE WRK-INP-FASE-DOTM-X     TO WRK-OUT-FASE-DOTM-X.
           MOVE WRK-INP-FIMAMOS-DOTM    TO WRK-OUT-FIMAMOS-DOTM.
           MOVE WRK-INP-NMCLIE-DOTM     TO WRK-OUT-NMCLIE-DOTM.
           MOVE WRK-INP-TIPO-PESQ-DOTM  TO WRK-OUT-TIPO-PESQ-DOTM.
           MOVE WRK-INP-TMSTAMP-DOTM    TO WRK-OUT-TMSTAMP-DOTM.
           MOVE WRK-INP-MENSA-DOTM      TO WRK-OUT-MENSA-DOTM.

           IF  WRK-INP-TIPO-PESQ-DOTM   EQUAL  1
               MOVE WRK-INP-BANCOX-DOTM     TO WRK-OUT-BANCOX-DOTM
               MOVE WRK-INP-AGENX-DOTM      TO WRK-OUT-AGENX-DOTM
               MOVE WRK-INP-CONTAX-DOTM     TO WRK-OUT-CONTAX-DOTM
               MOVE WRK-INP-BANCO-DOTM-X    TO WRK-OUT-BANCO-DOTM-X
               MOVE WRK-INP-AGENCIA-GR-DOTM TO WRK-OUT-AGENCIA-GR-DOTM
               MOVE WRK-INP-CONTA-DOTM      TO WRK-OUT-CONTA-DOTM
           ELSE
               MOVE WRK-INP-CNPJX-DOTM      TO WRK-OUT-CNPJX-DOTM
               MOVE WRK-INP-CNPJCPF-DOTM    TO WRK-OUT-CNPJCPF-DOTM
           END-IF.

      *----------------------------------------------------------------*
       1310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS VARIAVEIS DA TELA S/ ALTERACAO   *
      *----------------------------------------------------------------*
       1311-DEVOLVER-TELA-VARIA        SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
              IF  WRK-INP-CONTR-DOTM-X(WRK-IND)   IS  NUMERIC  AND
                  WRK-INP-CONTR-DOTM-X(WRK-IND)   GREATER  ZERO
                  MOVE WRK-INP-SELEC-DOTM (WRK-IND)
                                      TO WRK-OUT-SELEC-DOTM (WRK-IND)
                  MOVE WRK-INP-CONTR-DOTM-X(WRK-IND)
                                      TO WRK-OUT-CONTR-DOTM-X(WRK-IND)
                  MOVE WRK-INP-NOME-DOTM(WRK-IND)
                                      TO WRK-OUT-NOME-DOTM(WRK-IND)
                  MOVE WRK-INP-DTINIC-DOTM(WRK-IND)
                                      TO WRK-OUT-DTINIC-DOTM(WRK-IND)
                  MOVE WRK-INP-DTVENCT-DOTM(WRK-IND)
                                      TO WRK-OUT-DTVENCT-DOTM(WRK-IND)
                  MOVE WRK-INP-LIMITE-DOTM(WRK-IND)
                                      TO WRK-OUT-LIMITE-DOTM-X(WRK-IND)
                  MOVE WRK-INP-SITUAC-DOTM(WRK-IND)
                                      TO WRK-OUT-SITUAC-DOTM(WRK-IND)
              ELSE
                  MOVE WRK-241       TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
              END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1311-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1320-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0413'           TO WRK-TELA.
           MOVE 'DCOM0415'           TO COMU-TRANSACAO-413.
           MOVE SPACES               TO COMU-DADOS-413.
           MOVE WRK-INP-TMSTAMP-DOTM TO COMU-TMSTAMP-413.
           MOVE WRK-INP-TIPOOPC-DOTM TO COMU-OPCAO-413.

           MOVE COMU-AREA-413        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1330-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0415'          TO WRK-TELA
                                       COMU-TRANSACAO-413.
           MOVE '5'                 TO COMU-PFK-413.
           MOVE COMU-AREA-413       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1330-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1340-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0415'         TO WRK-TELA
                                      COMU-TRANSACAO-413.
           MOVE 'A'                TO COMU-PFK-413.
           MOVE COMU-AREA-413      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1350-TRATAR-FASE-1              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN WRK-INP-PFK-DOTM     EQUAL '7' OR
                   (WRK-INP-PFK-DOTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DOTM EQUAL 'PFK7')
                    PERFORM 1351-TRATAR-VOLTA-PAG

               WHEN WRK-INP-PFK-DOTM     EQUAL '8' OR
                   (WRK-INP-PFK-DOTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DOTM EQUAL 'PFK8')
                    PERFORM 1352-TRATAR-AVANCA-PAG

               WHEN WRK-INP-PFK-DOTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DOTM EQUAL  SPACES OR LOW-VALUES
                       PERFORM 1311-DEVOLVER-TELA-VARIA
                       PERFORM 1353-TRATAR-ENTER

             WHEN  OTHER
                   PERFORM 1311-DEVOLVER-TELA-VARIA
                   MOVE SG-MENSAGEM(38)
                                     TO  WRK-OUT-MENSA-DOTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1351-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-DOTM-N  EQUAL  1
               PERFORM 1311-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM (0031)   TO WRK-OUT-MENSA-DOTM
               GO TO 1351-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DOTM      NOT EQUAL WRK-MENSA-PF7

               MOVE ZEROS            TO WRK-QTD-D
                                        WRK-QTD-L

               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 5

                   IF  WRK-INP-SELEC-DOTM(WRK-IND) EQUAL 'D'
                       ADD  1          TO WRK-QTD-D
                   ELSE
                       IF  WRK-INP-SELEC-DOTM(WRK-IND) EQUAL 'L'
                           ADD  1          TO WRK-QTD-L
                       END-IF
                   END-IF

               END-PERFORM

               IF  WRK-QTD-D      GREATER ZEROS OR
                   WRK-QTD-L      GREATER ZEROS
                   PERFORM 1311-DEVOLVER-TELA-VARIA
                   MOVE WRK-MENSA-PF7           TO  WRK-OUT-MENSA-DOTM
                   GO  TO 1351-99-FIM
               END-IF

           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABELA-DOTM

           IF  WRK-INP-TIPO-PESQ-DOTM   EQUAL   1
               COMPUTE WRK-4111E-RESTART =
                                       (WRK-INP-PAGINA-DOTM-N - 2) * 5
               PERFORM 1500-ACESSAR-DCOM4111
           ELSE
               COMPUTE WRK-4112E-RESTART =
                                       (WRK-INP-PAGINA-DOTM-N - 2) * 5
               PERFORM 1600-ACESSAR-DCOM4112
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DOTM-N = WRK-INP-PAGINA-DOTM-N - 1.

      *----------------------------------------------------------------*
       1351-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1352-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-FIMAMOS-DOTM  EQUAL  '*'
              PERFORM 1311-DEVOLVER-TELA-VARIA
              MOVE SG-MENSAGEM (0030)  TO WRK-OUT-MENSA-DOTM
              GO TO 1352-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DOTM      NOT EQUAL WRK-MENSA-PF8

               MOVE ZEROS            TO WRK-QTD-D
                                        WRK-QTD-L

               PERFORM VARYING WRK-IND FROM 1 BY 1
                       UNTIL   WRK-IND GREATER 5

                   IF  WRK-INP-SELEC-DOTM(WRK-IND) EQUAL 'D'
                       ADD  1          TO WRK-QTD-D
                   ELSE
                       IF  WRK-INP-SELEC-DOTM(WRK-IND) EQUAL 'L'
                           ADD  1          TO WRK-QTD-L
                       END-IF
                   END-IF

               END-PERFORM

               IF  WRK-QTD-D      GREATER ZEROS OR
                   WRK-QTD-L      GREATER ZEROS
                   PERFORM 1311-DEVOLVER-TELA-VARIA
                   MOVE WRK-MENSA-PF8           TO  WRK-OUT-MENSA-DOTM
                   GO  TO 1352-99-FIM
               END-IF

           END-IF.
           MOVE  SPACES                TO WRK-OUT-TABELA-DOTM

           IF  WRK-INP-TIPO-PESQ-DOTM   EQUAL   1
               COMPUTE WRK-4111E-RESTART =
                                        WRK-INP-PAGINA-DOTM-N * 5
               PERFORM 1500-ACESSAR-DCOM4111
           ELSE
               COMPUTE WRK-4112E-RESTART =
                                        WRK-INP-PAGINA-DOTM-N * 5
               PERFORM 1600-ACESSAR-DCOM4112
           END-IF.

           COMPUTE WRK-OUT-PAGINA-DOTM-N = WRK-INP-PAGINA-DOTM-N + 1.

      *----------------------------------------------------------------*
       1352-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO 4NTER              *
      *----------------------------------------------------------------*
       1353-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO WRK-INCONSIS

           PERFORM 1354-CONSISTIR-FISICA

           IF WRK-INCONSIS        EQUAL  '*'
              GO TO 1353-99-FIM
           END-IF.

           PERFORM  VARYING  WRK-IND  FROM  1 BY 1
                      UNTIL  WRK-IND  GREATER 05
              IF  WRK-INP-SELEC-DOTM(WRK-IND)  EQUAL 'D'
                  PERFORM 1362-CHAMAR-TRAN-DCOM0416
                  GO   TO 1353-99-FIM
              END-IF
           END-PERFORM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND > 5
                   MOVE WRK-225    TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
           END-PERFORM.

           MOVE 2                      TO WRK-OUT-FASE-DOTM-N

           MOVE 'TECLE <PF6> PARA EFETIVAR A LIBERACAO OU <ENTER> PARA A
      -         'LTERAR'               TO  WRK-OUT-MENSA-DOTM.

      *----------------------------------------------------------------*
       1353-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1354-CONSISTIR-FISICA           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
             IF  WRK-INP-SELEC-DOTM(WRK-IND) NOT EQUAL 'L' AND 'D'  AND
                                                 SPACES  AND LOW-VALUES
                 MOVE '*'        TO WRK-INCONSIS
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
                 MOVE SG-MENSAGEM (0111)
                                 TO  WRK-OUT-MENSA-DOTM
                 GO TO 1354-99-FIM
             END-IF
           END-PERFORM.

           MOVE  ZERO                TO  WRK-IND   WRK-QTD-SPACES.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
             IF  WRK-INP-SELEC-DOTM(WRK-IND)   EQUAL SPACES
                 ADD 1           TO WRK-QTD-SPACES
             END-IF
           END-PERFORM.

           IF  WRK-QTD-SPACES                EQUAL 5
               MOVE '*'        TO WRK-INCONSIS
               MOVE SG-MENSAGEM (0053)
                               TO WRK-OUT-MENSA-DOTM
               GO TO 1354-99-FIM
           END-IF.

           MOVE  ZERO                TO  WRK-QTD-L   WRK-QTD-D.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 05
             IF  WRK-INP-SELEC-DOTM(WRK-IND) EQUAL 'L'
                 ADD 1               TO  WRK-QTD-L
             ELSE
                 IF  WRK-INP-SELEC-DOTM(WRK-IND) EQUAL 'D'
                     ADD 1           TO  WRK-QTD-D
                 END-IF
             END-IF
           END-PERFORM.

           IF (WRK-QTD-L           GREATER  ZEROS AND
               WRK-QTD-D           GREATER  ZEROS)
               MOVE '*'                 TO WRK-INCONSIS
               MOVE SG-MENSAGEM (0112)  TO  WRK-OUT-MENSA-DOTM
               PERFORM VARYING WRK-IND FROM 1 BY 1
                                               UNTIL WRK-IND GREATER 5
                    IF WRK-INP-SELEC-DOTM(WRK-IND) EQUAL 'L' OR 'D'
                       MOVE WRK-49353
                                    TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
                    END-IF
               END-PERFORM
           END-IF.

           IF WRK-INCONSIS        EQUAL  '*'
              GO TO 1354-99-FIM
           END-IF.

           IF  WRK-QTD-D           GREATER    1
               MOVE '*'                 TO WRK-INCONSIS
               MOVE SG-MENSAGEM (0113)  TO WRK-OUT-MENSA-DOTM
               PERFORM VARYING WRK-IND FROM 1 BY 1
                                               UNTIL WRK-IND GREATER 5
                    IF WRK-INP-SELEC-DOTM(WRK-IND) EQUAL 'D'
                       MOVE WRK-49353
                                     TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
                    END-IF
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       1354-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1360-TRATAR-FASE-2              SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN WRK-INP-PFK-DOTM     EQUAL '6' OR
                   (WRK-INP-PFK-DOTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DOTM EQUAL 'PFK6')
                    PERFORM 1361-PF6-CONFIRMA

               WHEN  WRK-INP-PFK-DOTM     EQUAL '.' AND
                     WRK-INP-COMANDO-DOTM EQUAL  SPACES OR LOW-VALUES
                     PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                           MOVE WRK-49345
                                     TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
                     END-PERFORM
                     PERFORM 1311-DEVOLVER-TELA-VARIA
                     MOVE  1         TO WRK-OUT-FASE-DOTM-N
               MOVE 'SELECIONE UM REGISTRO COM "D" OU VARIOS COM "L" E T
      -        'ECLE <ENTER>'        TO  WRK-OUT-MENSA-DOTM

               WHEN  OTHER
                     PERFORM 1311-DEVOLVER-TELA-VARIA
                     PERFORM VARYING WRK-IND FROM 1 BY 1
                                                      UNTIL WRK-IND > 5
                           MOVE WRK-225
                                     TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
                     END-PERFORM
                     MOVE SG-MENSAGEM(38)
                                     TO  WRK-OUT-MENSA-DOTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1360-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1361-PF6-CONFIRMA               SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO WRK-INCONSIS

           PERFORM  VARYING  WRK-IND  FROM  1 BY 1
                      UNTIL  WRK-IND  GREATER 05
              IF  WRK-INP-SELEC-DOTM(WRK-IND)  EQUAL 'L'
                  PERFORM 1363-CHAMAR-MOD-DCOM5142
              END-IF
           END-PERFORM.

           IF  WRK-LIBEROU-OK      EQUAL   'S'
               IF  WRK-INP-TIPO-PESQ-DOTM  EQUAL  1
                   MOVE ZEROS      TO  WRK-4111E-RESTART
                   PERFORM 1500-ACESSAR-DCOM4111
               ELSE
                   MOVE ZEROS      TO  WRK-4112E-RESTART
                   PERFORM 1600-ACESSAR-DCOM4112
               END-IF
               MOVE 1              TO  WRK-INP-PAGINA-DOTM-N
                                       WRK-OUT-PAGINA-DOTM-N
                                       WRK-INP-FASE-DOTM-N
                                       WRK-OUT-FASE-DOTM-N
               MOVE 'LIBERACOE(S) EFETUADA COM SUCESSO'
                                   TO  WRK-OUT-MENSA-DOTM

               PERFORM VARYING WRK-IND FROM 1 BY 1
                                  UNTIL WRK-IND > 5
                       MOVE WRK-49345
                                   TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
               END-PERFORM

           END-IF.

      *----------------------------------------------------------------*
       1361-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0416              *
      *----------------------------------------------------------------*
       1362-CHAMAR-TRAN-DCOM0416       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM0416'                  TO WRK-TELA.
           MOVE 'DCOM0415'                  TO COMU-TRANSACAO-416.
           MOVE WRK-INP-TIPOOPC-DOTM        TO COMU-OPCAO-416.
           MOVE WRK-INP-SENHAS-DOTM         TO COMU-SENHAS-416.
           MOVE WRK-INP-COMANDO-DOTM        TO COMU-COMANDO-416.
           MOVE WRK-INP-TMSTAMP-DOTM        TO COMU-TMSTAMP-416.
           MOVE WRK-INP-CONTR-DOTM(WRK-IND) TO COMU-CONTRATO-416.
           MOVE WRK-INP-PAGINA-DOTM-N       TO COMU-PAGINA-416.

           IF WRK-INP-AGENCIA-GR-DOTM  NOT EQUAL  SPACES
              MOVE WRK-INP-CODAGE-DOTM      TO COMU-CODAGE-416-N
              MOVE WRK-INP-CONTA-DOTM       TO COMU-CONTA-416-N
              MOVE SPACES                   TO COMU-NUMERO-416-X
                                               COMU-FILIAL-416-X
                                               COMU-CONTROLE-416-X
           ELSE
              MOVE WRK-INP-NUMERO-DOTM      TO COMU-NUMERO-416-N
              MOVE WRK-INP-FILIAL-DOTM      TO COMU-FILIAL-416-N
              MOVE WRK-INP-CONTROLE-DOTM    TO COMU-CONTROLE-416-N
              MOVE SPACES                   TO COMU-CODAGE-416-X
                                               COMU-CONTA-416-X
           END-IF.

           MOVE COMU-AREA-416               TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1362-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA MODULO DCOM5142             *
      *----------------------------------------------------------------*
       1363-CHAMAR-MOD-DCOM5142        SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-5142E-COD-RETORNO
           MOVE SPACES                 TO WRK-5142E-MSG-RETORNO
           MOVE ZEROS                  TO WRK-5142E-RESTART
           MOVE SPACES                 TO WRK-5142E-FLAG
           MOVE WRK-COD-USER           TO WRK-5142E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5142E-CTERM
           MOVE WRK-INP-CONTR-DOTM(WRK-IND)
                                       TO WRK-5142E-CCONTR-LIM-DESC
           MOVE 'DCOM5142'             TO WRK-MODULO

           CALL WRK-MODULO    USING    WRK-5142E-ENTRADA
                                       WRK-5142S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-5142S-COD-RETORNO
               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '0000'
                    MOVE  'S'      TO  WRK-LIBEROU-OK

               WHEN OTHER
                    PERFORM 1311-DEVOLVER-TELA-VARIA
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                                                  UNTIL WRK-IND > 5
                       MOVE WRK-225
                                   TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
                    END-PERFORM
                    MOVE  WRK-5142S-MSG-RETORNO  TO WRK-OUT-MENSA-DOTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1363-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4111                           *
      *----------------------------------------------------------------*
       1500-ACESSAR-DCOM4111           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4111E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4111E-CTERM
           MOVE WRK-INP-CODAGE-DOTM    TO  WRK-4111E-CAG-BCRIA
           MOVE WRK-INP-CONTA-DOTM     TO  WRK-4111E-CCTA-BCRIA
           MOVE 'L'                    TO  WRK-4111E-FLAG-FILTRO

           MOVE 'DCOM4111'             TO  WRK-MODULO.

           CALL WRK-MODULO    USING    WRK-4111E-ENTRADA
                                       WRK-4111S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4111S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4111S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4111S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DOTM
                       MOVE SG-MENSAGEM (0017)
                                    TO  WRK-OUT-MENSA-DOTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DOTM
                       MOVE SG-MENSAGEM (0001)
                                    TO  WRK-OUT-MENSA-DOTM
                    END-IF
                    PERFORM 1510-MONTAR-TELA-AGENCTA VARYING  WRK-IND
                                FROM  1 BY 1 UNTIL  WRK-IND  GREATER 05
               WHEN '0003'
                   INITIALIZE  COMU-AREA
                   IF  WRK-LIBEROU-OK      EQUAL   'S'
                       MOVE  SG-MENSAGEM(202)        TO COMU-MENSAG
                       PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
                   ELSE
                       MOVE  SG-MENSAGEM(197)        TO COMU-MENSAG
                       PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
                   END-IF

               WHEN '0088'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   INITIALIZE  COMU-AREA
                   MOVE  WRK-4111S-MSG-RETORNO   TO COMU-MENSAG
                   PERFORM 1150-RETORNA-CHAMADOR-DCOM0413

           END-EVALUATE.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA A PARTIR DO DCOM4111    *
      *----------------------------------------------------------------*
       1510-MONTAR-TELA-AGENCTA        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES             TO WRK-OUT-SELEC-DOTM(WRK-IND)

           IF  WRK-4111S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC AND
               WRK-4111S-CCONTR-LIM-DESC(WRK-IND) GREATER ZERO
               MOVE SPACES        TO WRK-OUT-SELEC-DOTM(WRK-IND)
               MOVE WRK-4111S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DOTM(WRK-IND)
               MOVE WRK-4111S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DOTM(WRK-IND)
               MOVE WRK-4111S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DOTM(WRK-IND)
               INSPECT WRK-OUT-DTINIC-DOTM(WRK-IND) REPLACING ALL
                                                     '.' BY '/'
               MOVE WRK-4111S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DOTM(WRK-IND)
               INSPECT WRK-OUT-DTVENCT-DOTM(WRK-IND) REPLACING ALL
                                                     '.' BY '/'
               MOVE WRK-4111S-VCONTR-LIM(WRK-IND)  TO WRK-EDT-VAL
               MOVE WRK-EDT-VAL   TO WRK-OUT-LIMITE-DOTM-X(WRK-IND)
               MOVE WRK-4111S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DOTM(WRK-IND)
           ELSE
               MOVE WRK-241       TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CHAMA O MODULO DCOM4112                           *
      *----------------------------------------------------------------*
       1600-ACESSAR-DCOM4112           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4112E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4112E-CTERM
           MOVE WRK-INP-NUMERO-DOTM    TO WRK-4112E-CCNPJ-CPF
           MOVE WRK-INP-FILIAL-DOTM    TO WRK-4112E-CFLIAL-CNPJ
           MOVE WRK-INP-CONTROLE-DOTM  TO WRK-4112E-CCTRL-CNPJ-CPF
           MOVE 'L'                    TO  WRK-4112E-FLAG-FILTRO
           MOVE WRK-COD-DEPTO-N        TO  WRK-AGENCIA-NUM.
           MOVE WRK-AGENCIA-AUX        TO  WRK-4112E-MSG-RETORNO(1:5).
           MOVE 'DCOM4112'             TO  WRK-MODULO.

           CALL WRK-MODULO    USING    WRK-4112E-ENTRADA
                                       WRK-4112S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4112S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4112S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4112S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DOTM
                       MOVE SG-MENSAGEM (0017)
                                    TO  WRK-OUT-MENSA-DOTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DOTM
                       MOVE SG-MENSAGEM (0001)
                                    TO  WRK-OUT-MENSA-DOTM
                    END-IF
                    PERFORM 1610-MONTAR-TELA-CNPJCPF

               WHEN '0003'
                    INITIALIZE COMU-AREA
                    IF  WRK-LIBEROU-OK      EQUAL   'S'
                        MOVE  SG-MENSAGEM(202)        TO COMU-MENSAG
                        PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
                    ELSE
                        MOVE  SG-MENSAGEM(197)        TO COMU-MENSAG
                        PERFORM 1150-RETORNA-CHAMADOR-DCOM0413
                    END-IF

               WHEN '0088'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN '0915'
                    INITIALIZE COMU-AREA
                    MOVE  WRK-4112S-MSG-RETORNO     TO COMU-MENSAG

                    IF  WRK-INP-TIPO-PESQ-DOTM  EQUAL  1
                        MOVE WRK-INP-CODAGE-DOTM    TO COMU-AGENCIA
                        MOVE WRK-INP-CONTA-DOTM     TO COMU-CONTA
                    ELSE
                        MOVE WRK-INP-CNPJCPF-DOTM(1:3)
                                                    TO COMU-CPF-X(1:3)
                        MOVE WRK-INP-CNPJCPF-DOTM(5:3)
                                                    TO COMU-CPF-X(4:3)
                        MOVE WRK-INP-CNPJCPF-DOTM(9:3)
                                                    TO COMU-CPF-X(7:3)
                        MOVE WRK-INP-FILIAL-DOTM    TO COMU-FILIAL
                        MOVE WRK-INP-CONTROLE-DOTM  TO COMU-DIGITO
                    END-IF

                    PERFORM 1150-RETORNA-CHAMADOR-DCOM0413

               WHEN OTHER
                    INITIALIZE COMU-AREA
                    MOVE  WRK-4112S-MSG-RETORNO   TO COMU-MENSAG
                    PERFORM 1150-RETORNA-CHAMADOR-DCOM0413

           END-EVALUATE.

      *----------------------------------------------------------------*
       1600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA A PARTIR DO DCOM4111    *
      *----------------------------------------------------------------*
       1610-MONTAR-TELA-CNPJCPF        SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM  1 BY 1
                      UNTIL  WRK-IND  GREATER 05

               MOVE SPACES             TO WRK-OUT-SELEC-DOTM(WRK-IND)
               IF  WRK-4112S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC AND
                   WRK-4112S-CCONTR-LIM-DESC(WRK-IND) GREATER ZERO
                   MOVE SPACES    TO WRK-OUT-SELEC-DOTM(WRK-IND)
                   MOVE WRK-4112S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DOTM(WRK-IND)
                   MOVE WRK-4112S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DOTM(WRK-IND)
                   MOVE WRK-4112S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DOTM(WRK-IND)
                   INSPECT WRK-OUT-DTINIC-DOTM(WRK-IND) REPLACING ALL
                                                     '.' BY '/'
                   MOVE WRK-4112S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DOTM(WRK-IND)
                   INSPECT WRK-OUT-DTVENCT-DOTM(WRK-IND) REPLACING ALL
                                                     '.' BY '/'
                   MOVE WRK-4112S-VCONTR-LIM(WRK-IND)
                                  TO WRK-EDT-VAL
                   MOVE WRK-EDT-VAL
                                  TO WRK-OUT-LIMITE-DOTM-X(WRK-IND)
                   MOVE WRK-4112S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DOTM(WRK-IND)
               ELSE
                  MOVE  SPACES       TO WRK-OUT-LINHAS-DOTM (WRK-IND)
                  MOVE WRK-241       TO WRK-OUT-SELEC-ATTR-DOTM(WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1700-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DOTM
                                             WRK-660-DCOMDOTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DOTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1700-99-FIM.                    EXIT.
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
               MOVE 'DCOM0415'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0415'         TO ERR-PGM
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
