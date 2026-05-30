      *----------------------------------------------------------------*
      * I#RUEC31                                                       *
      * LAY-OUT DA TABELA RUEC DE APOIO A ACESSOS - RUECB048       *   *
      *----------------------------------------------------------------*
       01  R31-AREA-TOTAL.
           03 R31-CHAVE.
              05 R31-CTERM                PIC  X(08).
              05 R31-CREG                 PIC  9(03)        COMP-3.
              05 R31-CSEQ-REG             PIC  9(05)        COMP-3.
           03 R31-TEXTO.
      *
      *          CREG = 000 - SELECAO PESSOAS CADASTRADAS(AREA-AUXILIAR)
      *                 001 - SELECAO PESSOAS CADASTRADAS(LAY-OUT = 000)
      *                       GRAVADO NO PGM RUEC7736
      *
              05 R31-NM-RAZSOC-001        PIC  X(70).
              05 R31-CCLUB-001            PIC  9(10).
              05 R31-CPF-CNPJ-CADU-001    PIC  9(15).
              05 R31-TP-PESSOA-001        PIC  X(01).
              05 R31-TITULRDD-001         PIC  9(02).
              05 R31-DNASC-FUND-001       PIC  X(10).
              05 R31-SIT-PESSOA-001.
                 07 R31-SIT-PESSOA-001-N  PIC  9(03).
              05 R31-CPF-CNPJ-RUEC-001    PIC  9(15).
MF0711        05 FILLER                   PIC  X(174).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 002 - SELECAO MENU IMOVEIS RURAIS (AREA AUXILIAR)
      *                003 - SELECAO MENU IMOVEIS RURAIS (LAY-OUT = 002)
      *                      GRAVADO NO PGM RUEC7722
      *
              05 R31-ESPEC-002            PIC  9(03).
              05 R31-RESPC-002            PIC  X(15).
PW1577        05 R31-RTPDOCTO-002         PIC  X(40).
              05 R31-CMUNIC-002           PIC  9(09).
              05 R31-RMUNIC-002           PIC  X(40).
              05 R31-DAQUIS-002           PIC  X(10).
              05 R31-UF-002               PIC  X(02).
              05 R31-DREG-002             PIC  X(10).
              05 R31-CRI-002              PIC  X(10).
              05 R31-CMATRIC-002          PIC  X(10).
              05 R31-CLIVRO-002           PIC  X(04).
              05 R31-CFOLHA-002           PIC  X(07).
PW1577        05 R31-MAREA-002            PIC  9(06)V9(04).
              05 R31-VATUAL-002           PIC  9(13)V9(02).
              05 R31-SQPATR-CADU-002      PIC  9(05).
              05 R31-SQPATR-RUEC-002      PIC  9(05).
              05 R31-SQIMOV-002           PIC  9(03).
              05 R31-ORIGEM-002           PIC  X(01).
              05 R31-RIMOV-002            PIC  X(25).
              05 R31-CIMPNH-002           PIC  X(01).
              05 R31-CINDMIGR-002         PIC  X(01).
              05 R31-CINDEXPDO-002        PIC  X(01).
              05 R31-CTPOESCRT-002        PIC  X(01).
              05 R31-CSITIMOV-002         PIC  9(01).
PW1577        05 R31-MAREACULTR-002       PIC  9(07)V9999.
PW1577        05 R31-MAREARBNHO-002       PIC  9(07)V9999.
PW1577        05 R31-MAREAOUTR-002        PIC  9(07)V9999.
              05 R31-RTPOEXPL-002         PIC  X(20).
MF0711        05 FILLER                   PIC  X(18).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 004 - PATRIMONIO IMOVEL - CADUB089
      *                      GRAVADO NO PGM RUEC7738
      *
              05 R31-CESPEC-004           PIC  9(03).
              05 R31-RESPEC-004           PIC  X(15).
              05 R31-VTOTIMOV-004         PIC  9(13)V99.
              05 R31-DAQUIS-004           PIC  X(10).
              05 R31-CMUN-004             PIC  9(09).
              05 R31-UF-004               PIC  X(02).
              05 R31-CMATR-004            PIC  X(10).
              05 R31-CINDEXPL-004         PIC  X(01).
              05 R31-DREGIMOV-004         PIC  X(10).
              05 R31-CRI-004              PIC  X(10).
              05 R31-CTPESCR-004          PIC  X(01).
              05 R31-CLIVRO-004           PIC  X(04).
              05 R31-CFOLHA-004           PIC  X(07).
              05 R31-CINDIMPNH-004        PIC  X(01).
              05 R31-MAREATOT-004         PIC  9(06)V99.
              05 R31-CSITIMOV-004         PIC  9(01).
              05 R31-RSITIMOV-004         PIC  X(20).
              05 R31-ICREDR-004           PIC  X(70).
              05 R31-VHIPOT-004           PIC  9(13)V99.
              05 R31-DVHIPOT-004          PIC  X(10).
MF0711        05 FILLER                   PIC  X(78).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 005 - PATRIMONIO VEICULO - CADUB091
      *                      GRAVADO NO PGM RUEC7738
      *
              05 R31-CVEIC-005            PIC  9(01).
              05 R31-RVEIC-005            PIC  X(20).
              05 R31-CMARCA-005           PIC  9(03).
              05 R31-RMARCA-005           PIC  X(20).
              05 R31-VATUAL-005           PIC  9(13)V99.
              05 R31-DANOVEIC-005         PIC  9(04).
              05 R31-CINDALIEN-005        PIC  X(01).
              05 R31-DMAVALIEN-005        PIC  9(06).
MF0711        05 FILLER                   PIC  X(230).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 006 - LEASING - CADUB076
      *                      GRAVADO NO PGM RUEC7738
      *
              05 R31-CBEM-006             PIC  9(02).
              05 R31-RBEM-006             PIC  X(20).
              05 R31-QBEM-006             PIC  9(05).
              05 R31-VCONTR-006           PIC  9(13)V99.
              05 R31-IEMPR-006            PIC  X(70).
              05 R31-DINIC-006            PIC  X(10).
              05 R31-DFIM-006             PIC  X(10).
MF0711        05 FILLER                   PIC  X(168).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 007 - SEGURO - CADUB0B6
      *                      GRAVADO NO PGM RUEC7738
      *
              05 R31-CMODLD-007           PIC  9(02).
              05 R31-RMODLD-007           PIC  X(20).
              05 R31-VTOTSEG-007          PIC  9(13)V99.
              05 R31-DAMSEG-007           PIC  9(06).
MF0711        05 FILLER                   PIC  X(257).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 008 - QUADRO SOCIAL (SOCIOS) - CADUB0A6
      *                      GRAVADO NO PGM RUEC7738
      *
              05 R31-DMAPARTCP-008        PIC  9(06).
              05 R31-IPARTCP-008          PIC  X(70).
              05 R31-NUM-CGC-008          PIC  9(09).
              05 R31-FIL-CGC-008          PIC  9(04).
              05 R31-CTR-CGC-008          PIC  9(02).
              05 R31-CNAC-008             PIC  9(03).
              05 R31-CCARGO-008           PIC  9(05).
              05 R31-RCARGO-008           PIC  X(20).
MF0711        05 FILLER                   PIC  X(181).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 009 - QUADRO SOCIAL (ADMNISTR.) - CADUB0A6
      *                      GRAVADO NO PGM RUEC7738
      *
              05 R31-DMAPARTCP-009        PIC  9(06).
              05 R31-IPARTCP-009          PIC  X(70).
              05 R31-NUM-CGC-009          PIC  9(09).
              05 R31-FIL-CGC-009          PIC  9(04).
              05 R31-CTR-CGC-009          PIC  9(02).
              05 R31-CNAC-009             PIC  9(03).
              05 R31-CCARGO-009           PIC  9(05).
              05 R31-RCARGO-009           PIC  X(20).
MF0711        05 FILLER                   PIC  X(181).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 010 - ERROS DETECTADOS NA VALIDACAO CAD. RURAL
      *                      GRAVADO NO PGM RUEC7718
      *
              05 R31-IERRO-010            PIC  X(79).
MF0711        05 FILLER                   PIC  X(221).

           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 011 - PATRIMONIO IMOVEL - CADUB089
      *         CREG = 012 - IMOVEL ARRENDADO  - RUECB055
      *                      GRAVADO NO PGM RUEC7742
      *
              05 R31-RIMOV-011            PIC  X(25).
              05 R31-CESPEC-011           PIC  9(03).
              05 R31-RESPEC-011           PIC  X(15).
              05 R31-VTOTIMOV-011         PIC  9(13)V99.
              05 R31-DAQUIS-011           PIC  X(10).
              05 R31-CMUN-011             PIC  9(09).
              05 R31-UF-011               PIC  X(02).
              05 R31-CMATR-011            PIC  X(10).
              05 R31-CINDEXPL-011         PIC  X(01).
              05 R31-DREGIMOV-011         PIC  X(10).
              05 R31-CRI-011              PIC  X(10).
              05 R31-CTPESCR-011          PIC  X(01).
              05 R31-CLIVRO-011           PIC  X(04).
              05 R31-CFOLHA-011           PIC  X(07).
              05 R31-CINDIMPNH-011        PIC  X(01).
PW1577        05 R31-MAREATOT-011         PIC  9(06)V9999.
              05 R31-CSITIMOV-011         PIC  9(01).
              05 R31-RSITIMOV-011         PIC  X(20).
              05 R31-ICREDR-011           PIC  X(70).
              05 R31-VHIPOT-011           PIC  9(13)V99.
              05 R31-DVHIPOT-011          PIC  X(10).
MF0711        05 FILLER                   PIC  X(51).

PW1577     03 FILLER     REDEFINES  R31-TEXTO.
PW1577*
PW1577*         CREG = 013 - LISTAS DE APOIO - CLASSIFICADA POR CODIGO
PW1577*         CREG = 014 - LISTAS DE APOIO - CLASSIFICADA POR DESCRICA
PW1577*                      GRAVADO NO PGM RUEC6950
PW1577*
PW1577        05 R31-DESCR-APOIO-013      PIC  X(70).
PW1577        05 R31-COD-APOIO-013        PIC  X(11).
PW1577        05 FILLER                   PIC  X(219).
PW1577
PW1577     03 FILLER     REDEFINES  R31-TEXTO.
PW1577*
PW1577*         CREG = 015 - QUALIFICACAO - AVALISTA
PW1577*         CREG = 016 - QUALIFICACAO - TERCEIRO PROPRIETARIO
PW1577*         CREG = 017 - QUALIFICACAO - FIEL DEPOSITARIO
PW1577*         CREG = 018 - QUALIFICACAO - EMITENTE SOLIDARIO
PW1577*                      GRAVADO NO PGM RUEC7270
PW1577*
PW1577*         BLOCO 1 - CGC/CPF - ENDERECO
PW1577*
PW1577        05 R31-CPF-CGC-BL1          PIC  9(09).
PW1577        05 R31-CGC-FILIAL-BL1       PIC  9(04).
PW1577        05 R31-CPF-CTRL-BL1         PIC  9(02).
PW1577        05 R31-NOME-BL1             PIC  X(60).
PW1577        05 R31-ENDERECO-BL1         PIC  X(40).
PW1577        05 R31-END-NRO-BL1          PIC  X(05).
PW1577        05 R31-END-COMPL-BL1        PIC  X(15).
PW1577        05 R31-CEP-BL1              PIC  9(05).
PW1577        05 R31-CEP-COMPL-BL1        PIC  9(03).
PW1577        05 R31-BAIRRO-BL1           PIC  X(20).
PW1577        05 R31-COD-MUNIC-BL1        PIC  9(09).
PW1577        05 R31-END-UF-BL1           PIC  X(02).
PW1577        05 R31-COD-ATIV-BL1         PIC  X(11).
PW1577        05 FILLER                   PIC  X(115).
PW1577
PW1577     03 FILLER     REDEFINES  R31-TEXTO.
PW1577*
PW1577*         CREG = 015 - QUALIFICACAO - AVALISTA
PW1577*         CREG = 016 - QUALIFICACAO - TERCEIRO PROPRIETARIO
PW1577*         CREG = 017 - QUALIFICACAO - FIEL DEPOSITARIO
PW1577*         CREG = 018 - QUALIFICACAO - EMITENTE SOLIDARIO
PW1577*                      GRAVADO NO PGM RUEC7270
PW1577*
PW1577*         BLOCO 2 - INFORMACOES PESSOAIS
PW1577*
PW1577        05 R31-SEXO-BL2             PIC  X(01).
PW1577        05 R31-EST-CIVIL-BL2        PIC  9(03).
PW1577        05 R31-DIA-UNIAO-BL2        PIC  9(02).
PW1577        05 R31-MES-UNIAO-BL2        PIC  9(02).
PW1577        05 R31-ANO-UNIAO-BL2        PIC  9(04).
PW1577        05 R31-COD-NAC-BL2          PIC  9(03).
PW1577        05 R31-NATURAL-BL2          PIC  X(40).
PW1577        05 R31-NAT-UF-BL2           PIC  X(02).
PW1577        05 R31-TPO-DOCTO-BL2        PIC  9(03).
PW1577        05 R31-COD-DOCTO-BL2        PIC  X(15).
PW1577        05 R31-ORG-EMIS-BL2         PIC  X(04).
PW1577        05 R31-ORG-EMIS-UF-BL2      PIC  X(02).
PW1577        05 R31-CATEG-PROF-BL2       PIC  9(05).
PW1577        05 R31-COD-PROFIS-BL2       PIC  9(05).
PW1577        05 FILLER                   PIC  X(209).
PW1577
PW1577     03 FILLER     REDEFINES  R31-TEXTO.
PW1577*
PW1577*         CREG = 015 - QUALIFICACAO - AVALISTA
PW1577*         CREG = 016 - QUALIFICACAO - TERCEIRO PROPRIETARIO
PW1577*         CREG = 017 - QUALIFICACAO - FIEL DEPOSITARIO
PW1577*         CREG = 018 - QUALIFICACAO - EMITENTE SOLIDARIO
PW1577*                      GRAVADO NO PGM RUEC7280
PW1577*
PW1577*         BLOCO 3 - INFORMACOES DO CONJUGE
PW1577*
PW1577        05 R31-CPF-CGC-BL3          PIC  9(09).
PW1577        05 R31-CPF-CTRL-BL3         PIC  9(02).
PW1577        05 R31-NOME-BL3             PIC  X(60).
PW1577        05 R31-COD-NAC-BL3          PIC  9(03).
PW1577        05 R31-NATURAL-BL3          PIC  X(40).
PW1577        05 R31-NAT-UF-BL3           PIC  X(02).
PW1577        05 R31-TPO-DOCTO-BL3        PIC  9(03).
PW1577        05 R31-COD-DOCTO-BL3        PIC  X(15).
PW1577        05 R31-ORG-EMIS-BL3         PIC  X(04).
PW1577        05 R31-ORG-EMIS-UF-BL3      PIC  X(02).
PW1577        05 R31-CATEG-PROF-BL3       PIC  9(05).
PW1577        05 R31-COD-PROFIS-BL3       PIC  9(05).
PW1577        05 FILLER                   PIC  X(150).
PW1577
PW1577     03 FILLER     REDEFINES  R31-TEXTO.
PW1577*
PW1577*         CREG = 019 - TERCEIROS PROPRIETARIOS-
PW1577*                      GRAVADO NO PGM RUEC7260
PW1577*
PW1577        05 R31-CGCCPF-019           PIC 9(09) COMP-3.
PW1577        05 R31-FILIAL-019           PIC 9(04).
PW1577        05 R31-CTRL-CPF-019         PIC 9(02).
PW1577        05 R31-NOME-019             PIC X(60).
PW1577        05 R31-SITU-019             PIC X(11).
PW1577        05 R31-SEQU-019             PIC 9(03).
PW1577        05 R31-ESTADO-019           PIC X(01).
PW1577        05 R31-QUALIFIC-019         PIC X(01).
PW1577        05 R31-GRUPO1-019           PIC 9(05).
PW1577        05 R31-GRUPO2-019           PIC 9(05).
PW1577        05 R31-GRUPO3-019           PIC 9(05).
PW1577        05 FILLER                   PIC X(198).

PW1577     03 FILLER     REDEFINES  R31-TEXTO.
PW1577*
PW1577*         CREG = 020 - QUALIFICACAO - BLOCO 1 (P/ NAVEGACAO)
PW1577*         SEQ  = 001 - GRAVADO NO PGM RUEC7270 E RUEC8270
PW1577*
PW1577        05 R31-TITULO1-020          PIC  X(24).
PW1577        05 R31-CPF-CGC1-020         PIC  X(09).
PW1577        05 R31-CGC-FILIAL1-020      PIC  X(04).
PW1577        05 R31-CPF-CTRL1-020        PIC  X(02).
PW1577        05 R31-NOME1-020            PIC  X(60).
PW1577        05 R31-ENDERECO-020         PIC  X(40).
PW1577        05 R31-END-NRO-020          PIC  X(05).
PW1577        05 R31-END-COMPL-020        PIC  X(15).
PW1577        05 R31-CEP-020              PIC  X(05).
PW1577        05 R31-CEP-COMPL-020        PIC  X(03).
PW1577        05 R31-BAIRRO-020           PIC  X(20).
PW1577        05 R31-COD-MUNIC-020        PIC  X(09).
PW1577        05 R31-DES-MUNIC-020        PIC  X(30).
PW1577        05 R31-END-UF-020           PIC  X(02).
PW1577        05 R31-SEL-ATIV-020         PIC  X(01).
PW1577        05 R31-DES-ATIV-020         PIC  X(55).
PW1577        05 FILLER                   PIC  X(16).

PW1577     03 FILLER     REDEFINES  R31-TEXTO.
PW1577*
PW1577*         CREG = 020 - QUALIFICACAO - BLOCO 2 (P/ NAVEGACAO)
PW1577*         SEQ  = 002 - GRAVADO NO PGM RUEC7270 E RUEC8270
PW1577*
PW1577        05 R31-RAMO-020             PIC  X(11).
PW1577        05 R31-SEXO-020             PIC  X(01).
PW1577        05 R31-EST-CIVIL-020        PIC  X(09).
PW1577        05 R31-DESC-ECIV-020        PIC  X(20).
PW1577        05 R31-DIA-UNIAO-020        PIC  X(02).
PW1577        05 R31-MES-UNIAO-020        PIC  X(02).
PW1577        05 R31-ANO-UNIAO-020        PIC  X(04).
PW1577        05 R31-COD-NAC1-020         PIC  X(03).
PW1577        05 R31-DES-NAC1-020         PIC  X(20).
PW1577        05 R31-NATURAL1-020         PIC  X(40).
PW1577        05 R31-NAT-UF1-020          PIC  X(02).
PW1577        05 R31-TPO-DOCTO1-020       PIC  X(03).
PW1577        05 R31-COD-DOCTO1-020       PIC  X(15).
PW1577        05 R31-ORG-EMIS1-020        PIC  X(04).
PW1577        05 R31-ORG-EMIS-UF1-020     PIC  X(02).
PW1577        05 R31-CATEG-PROF1-020      PIC  X(05).
PW1577        05 R31-DES-CATPRF1-020      PIC  X(49).
PW1577        05 R31-COD-PROFIS1-020      PIC  X(05).
PW1577        05 R31-DES-CODPRF1-020      PIC  X(62).
PW1577        05 R31-CONJUGE-020          PIC  X(01).
PW1577        05 FILLER                   PIC  X(40).

PW1577     03 FILLER     REDEFINES  R31-TEXTO.
PW1577*
PW1577*         CREG = 020 - QUALIFICACAO - BLOCO 2 (P/ NAVEGACAO)
PW1577*         SEQ  = 003 - GRAVADO NO PGM RUEC7280 E RUEC8280
PW1577*
PW1577        05 R31-TITULO3-020          PIC  X(24).
PW1577        05 R31-CPF-CGC3-020         PIC  X(09).
PW1577        05 R31-CPF-CTRL3-020        PIC  X(02).
PW1577        05 R31-NOME3-020            PIC  X(60).
PW1577        05 R31-COD-NAC3-020         PIC  X(03).
PW1577        05 R31-DES-NAC3-020         PIC  X(20).
PW1577        05 R31-NATURAL3-020         PIC  X(40).
PW1577        05 R31-NAT-UF3-020          PIC  X(02).
PW1577        05 R31-TPO-DOCTO3-020       PIC  X(03).
PW1577        05 R31-COD-DOCTO3-020       PIC  X(15).
PW1577        05 R31-ORG-EMIS3-020        PIC  X(04).
PW1577        05 R31-ORG-EMIS-UF3-020     PIC  X(02).
PW1577        05 R31-CATEG-PROF3-020      PIC  X(05).
PW1577        05 R31-DES-CATPRF3-020      PIC  X(47).
PW1577        05 R31-COD-PROFIS3-020      PIC  X(05).
PW1577        05 R31-DES-CODPRF3-020      PIC  X(58).
PW1577        05 R31-IND-TABCGC-020       PIC  9(01).
PW1577
           03 FILLER     REDEFINES  R31-TEXTO.
PW2678*
PW2678*         CREG = 021 - SELECAO PESSOAS CADASTRADAS CORPO CNPJ
PW2678*         CREG = 022 - SELECAO PESSOAS CADASTRADAS CLASSIFICADA
PW2678*         CTERM = TERMINAL P/ ONLINE; PGM PRINC P/ BATCH
PW2678*                 USADO NO PGM RUEC7776
PW2678        05 R31-NM-RAZSOC-021        PIC  X(70).
PW2678        05 R31-CCLUB-021            PIC  9(10).
PW2678        05 R31-CPF-CNPJ-CADU-021    PIC  9(15).
PW2678        05 FILLER                   PIC  X(205).
      *
           03 FILLER     REDEFINES  R31-TEXTO.
PW2678*
PW2678*         CREG = 023 - SELECAO IMOVEIS CADASTRADOS CORPO CNPJ
PW2678*         CREG = 024 - SELECAO IMOVEIS SUMARIZADOS
PW2678*         CTERM = TERMINAL P/ ONLINE; PGM PRINC P/ BATCH
PW2678*                 USADO NO PGM RUEC7778
PW2678        05 R31-QTDE-IMOV-023        PIC  9(03).
PW2678        05 R31-COD-MUN-023          PIC  9(09).
PW2678        05 R31-DESCR-MUN-023        PIC  X(40).
MF0711        05 FILLER                   PIC  X(248).
      *
           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 025 - AREA DE SALVA TELA PGM RUEC7000/RUEC8000
      *
              05 R31-SALVA-RUEC7000       PIC  X(300).
      *
PW2875     03 FILLER     REDEFINES  R31-TEXTO.
PW2875*
PW2875*         CREG = 026 - CONSULTA EXCLUSAO RECOR
PW2875*
PW2875        05 R31-CEDULA-026           PIC  9(09).
PW2875        05 R31-CPF-CNPJ-026         PIC  X(17).
PW2875        05 R31-NOME-026             PIC  X(40).
PW2875        05 R31-AGENCIA-026          PIC  9(05).
PW2875        05 R31-DT-INCLUSAO-026      PIC  X(10).
PW2875        05 R31-DT-IMPRESSAO-026     PIC  X(10).
PW2875        05 R31-DT-REJEICAO-026      PIC  X(10).
PW2875        05 R31-VALOR-026            PIC  9(09)V99.
PW2875        05 R31-CHAVE-026            PIC  9(09).
PW2875        05 R31-REF-BACEN-026        PIC  X(11).
PW2875        05 R31-FLAG-COMCZ-026       PIC  X(01).
PW2875        05 FILLER                   PIC  X(167).
      *
PW2875     03 FILLER     REDEFINES  R31-TEXTO.
PW2875*
PW2875*         CREG = 027 - PARAMETRO DE PRAZO DA DTA DE VENCIMENTO
PW2875*         SEQ  = 999 - NUMERO DA PAGINA
PW2875*
PW2875        05   R31-LINHA              OCCURS  10    TIMES.
PW2875          07 R31-ORIGEM-DE          PIC 9(03) COMP-3.
PW2875          07 R31-ORIGEM-ATE         PIC 9(03) COMP-3.
PW2875          07 R31-FINALI-DE          PIC 9(03) COMP-3.
PW2875          07 R31-FINALI-ATE         PIC 9(03) COMP-3.
PW2875          07 R31-ATIVID-DE          PIC 9(03) COMP-3.
PW2875          07 R31-ATIVID-ATE         PIC 9(03) COMP-3.
PW2875          07 R31-PRODUT-DE          PIC 9(03) COMP-3.
PW2875          07 R31-PRODUT-ATE         PIC 9(03) COMP-3.
PW2875          07 R31-ALTERA-VCTO        PIC X(01).
PW2875          07 R31-DIA-LIMITE         PIC 9(02).
PW2875          07 R31-MES-LIMITE         PIC 9(02).
PW2875          07 R31-ANO-LIMITE         PIC 9(04).
PW2875          07 R31-DIA-FIXO           PIC 9(02).
PW2875          07 R31-INCONSIS           PIC X(01).
MF0711        05 FILLER                   PIC X(20).
      *
MF0711     03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 028 - AREA DE SALVA TELA PGM RUEC7360
      *
MF0711        05 R31-SALVA-RUEC7360       PIC  X(300).
      *
PW9797     03 FILLER     REDEFINES  R31-TEXTO.
PW9797*
PW9797*         CREG = 029 - AREA DE SALVA TELA PGM RUEC7210
PW9797*
PW9797        05 R31-CLICEN-AMBTL         PIC X(015).
PW9797        05 R31-CTPO-LICEN-AMBTL     PIC X(002).
PW9797        05 R31-DFNAL-LICEN-AMBTL    PIC X(010).
PW9797        05 R31-IORG-CONCS-LICEN     PIC X(050).
PW9797        05 R31-CSGL-UF-CONCS        PIC X(002).
PW9797        05 R31-RFNALD-CONCS-LICEN   PIC X(100).
PW9797        05 R31-CINDCD-PORTE-BNEFC   PIC X(001).
PW9797        05 FILLER                   PIC X(120).
      *
PW9931     03 FILLER     REDEFINES  R31-TEXTO.
PW9931*
PW9931*         CREG = 030 E 031 - SISTEMA DE PRODUCAO/EMPREEND. RECOR
PW9931        05 FILLER                   PIC X(300).
      *
PW9931     03 FILLER     REDEFINES  R31-TEXTO.
PW9931*
PW9931*         CREG = 032 - PRODUTOS CONSORCIADOS
PW9931*         SEQ  = 999 - NUMERO DA PAGINA
PW9931*
PW9931        05   R31-PROD-CONS          OCCURS  99    TIMES.
PW9931          07 R31-PROD-CONSORCIADO   PIC 9(03).
PW9931        05 FILLER                   PIC X(03).
      *
PW9931     03 FILLER     REDEFINES  R31-TEXTO.
PW9931*
PW9931*         CREG = 033 - ERROS BACEN COR0001E E COR0002E
PW9931*
PW9931        05  R31-CERRO-BACEN         PIC X(08).
PW9931        05  R31-IERRO-BACEN         PIC X(80).
PW9931        05  FILLER                  PIC X(212).
      *
           03 FILLER     REDEFINES  R31-TEXTO.
      *
      *         CREG = 998 - AREA DE ENVIO NA CHAMADA DO MODULO
      *         CREG = 999 - AREA DE RETORNO NA CHAMADA DO MODULO
      *
      *         CTERM = TERMINAL P/ ONLINE; PGM PRINC P/ BATCH
      *         CSEQ  = 4 POS. NUMERO MODULO; 1 POS. SEQUENCIA REG.
      *
      *                 USADO NO PGM RUEC7746
              05 FILLER                   PIC  X(300).
