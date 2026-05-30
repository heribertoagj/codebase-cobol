      *****************************************************************
      *   BVVE - AFILIACAO ELETRONICA DE ESTABELECIMENTOS             *
      *---------------------------------------------------------------*
      *   I#BVVEWR - MOVIMENTO DE RETORNO VISANET - WORK-AREA         *
      *   TAMANHO  - 1920 BYTES                                       *
      *****************************************************************
      *
       01  BVVEWR-WORK-AREA.

      * IDENTIFICACAO DE ARQUIVO / ESTABELECIMENTO
           05 WR-MENSAGEM                          PIC 9(003).
           05 WR-MENSAGEM-R            REDEFINES  WR-MENSAGEM
                                                   PIC X(003).


           05 WR-IDENT-SOLCT.
              10 WR-NUM-INSTT-FINAN                PIC 9(004).
              10 WR-NUM-INSTT-FINAN-R  REDEFINES  WR-NUM-INSTT-FINAN
                                                   PIC X(004).
              10 WR-NUM-IDENT-CHAMD                PIC X(004).


      * RESERVADO VISANET
           05 FILLER                               PIC X(019).


      * CODIGO DE ERRO GENERICO
           05 WR-CERRO                             PIC 9(002).
           05 WR-CERRO-R               REDEFINES  WR-CERRO
                                                   PIC X(002).

      * CODIGO DE ERRO BANCO
      * BANCO
           05 WR-CERRO-CBANCO                      PIC 9(002).
           05 WR-CERRO-CBANCO-R        REDEFINES  WR-CERRO-CBANCO
                                                   PIC X(002).
           05 WR-CBANCO                            PIC 9(003).
           05 WR-CBANCO-R              REDEFINES  WR-CBANCO
                                                   PIC X(003).


      * NUMERO DO ESTABELECIMENTO
           05 WR-NUM-ESTBL                         PIC 9(010).
           05 WR-NUM-ESTBL-R           REDEFINES  WR-NUM-ESTBL
                                                   PIC X(010).


      * CODIGO DE ERRO ESTABELECIMENTO
      * ESTABELECIMENTO
           05 WR-CERRO-IESTBL                      PIC 9(002).
           05 WR-CERRO-IESTBL-R        REDEFINES  WR-CERRO-IESTBL
                                                   PIC X(002).
           05 WR-IESTBL                            PIC X(032).


      * CODIGO DE ERRO ENDERECO CORRESPONDENCIA
      * ENDERECO CORRESPONDENCIA
EMER       05 WR-CERRO-ENDER-1                    PIC 9(002).
EMER       05 WR-CERRO-ENDER-1-R      REDEFINES  WR-CERRO-ENDER-1
EMER                                              PIC X(002).
EMER       05 WR-EAFLIA-1                         PIC X(064).


      * CODIGO DE ERRO CIDADE CORRESPONDENCIA
      * CIDADE CORRESPONDENCIA
           05 WR-CERRO-CIDDE-1                     PIC 9(002).
           05 WR-CERRO-CIDDE-1-R       REDEFINES  WR-CERRO-CIDDE-1
                                                   PIC X(002).
           05 WR-IMUN-1                            PIC X(028).


      * CODIGO DE ERRO ESTADO CORRESPONDENCIA
      * ESTADO CORRESPONDENCIA
           05 WR-CERRO-CSGL-UF-1                   PIC 9(002).
           05 WR-CERRO-CSGL-UF-1-R     REDEFINES  WR-CERRO-CSGL-UF-1
                                                    PIC X(002).
           05 WR-CSGL-UF-1                         PIC X(002).


      * CODIGO DE ERRO CEP CORRESPONDENCIA
      * CEP CORRESPONDENCIA
           05 WR-CERRO-CCEP-1                      PIC 9(002).
           05 WR-CERRO-CCEP-1-R        REDEFINES  WR-CERRO-CCEP-1
                                                   PIC X(002).
           05 WR-CCEP-1                            PIC 9(008).
           05 WR-CCEP-1-R              REDEFINES  WR-CCEP-1
                                                    PIC X(008).


      * CODIGO DE ERRO CNPJ/CPF
      * CNPJ/CPF
           05 WR-CERRO-CCNPJ-CPF                   PIC 9(002).
           05 WR-CERRO-CCNPJ-CPF-R     REDEFINES  WR-CERRO-CCNPJ-CPF
                                                   PIC X(002).
           05 WR-CCNPJ-CPF                         PIC 9(015).
           05 WR-CCNPJ-CPF-R           REDEFINES  WR-CCNPJ-CPF
                                                    PIC X(015).


      * CODIGO DE ERRO INSCRICAO ESTADUAL
      * INSCRICAO ESTADUAL
           05 WR-CERRO-CINSCR-EST                   PIC 9(002).
           05 WR-CERRO-CINSCR-EST-R    REDEFINES  WR-CERRO-CINSCR-EST
                                                    PIC X(002).
           05 WR-CINSCR-EST                         PIC 9(015).
           05 WR-CINSCR-EST-R          REDEFINES  WR-CINSCR-EST
                                                    PIC X(015).


      * CODIGO DE ERRO NOME FANTASIA
      * NOME FANTASIA
           05 WR-CERRO-IFANTS                       PIC 9(002).
           05 WR-CERRO-IFANTS-R        REDEFINES  WR-CERRO-IFANTS
                                                    PIC X(002).
           05 WR-IFANTS-ESTBL                       PIC X(032).


      * CODIGO DE ERRO ENDERECO FISCAL
      * ENDERECO FISCAL
           05 WR-CERRO-ENDER-2                      PIC 9(002).
           05 WR-CERRO-ENDER-2-R       REDEFINES  WR-CERRO-ENDER-2
                                                    PIC X(002).
           05 WR-EAFLIA-2                           PIC X(064).


      * CODIGO DE ERRO CIDADE FISCAL
      * CIDADE FISCAL
           05 WR-CERRO-CIDDE-2                      PIC 9(002).
           05 WR-CERRO-CIDDE-2-R       REDEFINES  WR-CERRO-CIDDE-2
                                                    PIC X(002).
           05 WR-IMUN-2                             PIC X(028).


      * CODIGO DE ERRO ESTADO FISCAL
      * ESTADO FISCAL
           05 WR-CERRO-CSGL-UF-2                    PIC 9(002).
           05 WR-CERRO-CSGL-UF-2-R     REDEFINES  WR-CERRO-CSGL-UF-2
                                                    PIC X(002).
           05 WR-CSGL-UF-2                          PIC X(002).


      * CODIGO DE ERRO CEP FISCAL
      * CEP FISCAL
           05 WR-CERRO-CCEP-2                       PIC 9(002).
           05 WR-CERRO-CCEP-2-R        REDEFINES  WR-CERRO-CCEP-2
                                                    PIC X(002).
           05 WR-CCEP-2                             PIC 9(008).
           05 WR-CCEP-2-R              REDEFINES  WR-CCEP-2
                                                    PIC X(008).


      * CODIGO DE ERRO PLAQUETA
      * PLAQUETA
           05 WR-CERRO-IPLAQT                       PIC 9(002).
           05 WR-CERRO-IPLAQT-R        REDEFINES  WR-CERRO-IPLAQT
                                                    PIC X(002).
           05 WR-IPLAQT                             PIC X(022).


      * CODIGO DE ERRO PESSOA CONTATO
      * PESSOA CONTATO
           05 WR-CERRO-CNTAT                        PIC 9(002).
           05 WR-CERRO-CNTAT-R         REDEFINES  WR-CERRO-CNTAT
                                                    PIC X(002).
           05 WR-ICNTAT-ESTBL                       PIC X(032).


           05 WR-FILLER                             PIC X(032).
           05 WR-ZERO-1                             PIC 9(002).
           05 WR-ZERO-2                             PIC 9(010).


      * CODIGO DE ERRO RAMO ATIVIDADE
      * RAMO ATIVIDADE
           05 WR-CERRO-RAMO-ATVDD                   PIC 9(002).
           05 WR-CERRO-RAMO-ATVDD-R    REDEFINES  WR-CERRO-RAMO-ATVDD
                                                    PIC X(002).
           05 WR-CTPO-ATVDD                         PIC 9(005).
           05 WR-CTPO-ATVDD-R          REDEFINES  WR-CTPO-ATVDD
                                                    PIC X(005).


      * CODIGO DE ERRO AGENCIA
      * AGENCIA
           05 WR-CERRO-CAG                          PIC 9(002).
           05 WR-CERRO-CAG-R           REDEFINES  WR-CERRO-CAG
                                                    PIC X(002).
           05 WR-CAG-BCARIA                         PIC 9(005).
           05 WR-CAG-BCARIA-R          REDEFINES  WR-CAG-BCARIA
                                                    PIC X(005).

      * CODIGO DE ERRO CONTA CORRENTE
      * CONTA CORRENTE
           05 WR-CERRO-CORR                         PIC 9(002).
           05 WR-CERRO-CORR-R          REDEFINES  WR-CERRO-CORR
                                                    PIC X(002).
EMER       05 WR-CCTA-BCARIA                        PIC 9(014).
           05 WR-CCTA-BCARIA-R         REDEFINES  WR-CCTA-BCARIA
                                                    PIC X(014).

      * CODIGO DE ERRO NOME PROPRIETARIO 1
      * NOME PROPRIETARIO 1
           05 WR-CERRO-NOME-PPRIE-1                 PIC 9(002).
           05 WR-CERRO-NOME-PPRIE-1-R  REDEFINES  WR-CERRO-NOME-PPRIE-1
                                                    PIC X(002).
           05 WR-IPPRIE-1                           PIC X(032).

      * CODIGO DE ERRO CPF PROPRIETARIO 1
      * CPF PROPRIETARIO 1
           05 WR-CERRO-CPF-PPRIE-1                  PIC 9(002).
           05 WR-CERRO-CPF-PPRIE-1-R   REDEFINES  WR-CERRO-CPF-PPRIE-1
                                                    PIC X(002).
           05 WR-CPF-PPRIE-1                        PIC 9(015).
           05 WR-CPF-PPRIE-1-R         REDEFINES  WR-CPF-PPRIE-1
                                                    PIC X(015).


      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 1
      * DATA NASCIMENTO PROPRIETARIO 1
           05 WR-CERRO-DNASC-PPRIE                  PIC 9(002).
           05 WR-CERRO-DNASC-PPRIE-R   REDEFINES  WR-CERRO-DNASC-PPRIE
                                                    PIC X(002).
           05 WR-DNASC-PPRIE                        PIC 9(006).
           05 WR-DNASC-PPRIE-R         REDEFINES  WR-DNASC-PPRIE
                                                    PIC X(006).


      * CODIGO DE ERRO NOME PROPRIETARIO 2
      * NOME PROPRIETARIO 2
           05 WR-CERRO-NOME-PPRIE-2                 PIC 9(002).
           05 WR-CERRO-NOME-PPRIE-2-R  REDEFINES  WR-CERRO-NOME-PPRIE-2
                                                    PIC X(002).
           05 WR-IPPRIE-2                           PIC X(032).


      * CODIGO DE ERRO CPF PROPRIETARIO 2
      * CPF PROPRIETARIO 2
           05 WR-CERRO-CPF-PPRIE-2                  PIC 9(002).
           05 WR-CERRO-CPF-PPRIE-2-R   REDEFINES  WR-CERRO-CPF-PPRIE-2
                                                    PIC X(002).
           05 WR-CPF-PPRIE-2                        PIC 9(015).
           05 WR-CPF-PPRIE-2-R         REDEFINES  WR-CPF-PPRIE-2
                                                    PIC 9(015).


      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 2
      * DATA NASCIMENTO PROPRIETARIO 2
           05 WR-CERRO-DNASC-PPRIE-2                PIC 9(002).
           05 WR-CERRO-DNASC-PPRIE-2-R REDEFINES  WR-CERRO-DNASC-PPRIE-2
                                                    PIC X(002).
           05 WR-DNASC-PPRIE-2                      PIC 9(006).
           05 WR-DNASC-PPRIE-2-R       REDEFINES  WR-DNASC-PPRIE-2
                                                    PIC X(006).

      * CODIGO DE ERRO NOME PROPRIETARIO 3
      * NOME PROPRIETARIO 3
           05 WR-CERRO-NOME-PPRIE-3                 PIC 9(002).
           05 WR-CERRO-NOME-PPRIE-3-R  REDEFINES  WR-CERRO-NOME-PPRIE-3
                                                    PIC X(002).
           05 WR-IPPRIE-3                           PIC X(032).


      * CODIGO DE ERRO CPF PROPRIETARIO 3
      * CPF PROPRIETARIO 3
           05 WR-CERRO-CPF-PPRIE-3                  PIC 9(002).
           05 WR-CERRO-CPF-PPRIE-3-R   REDEFINES  WR-CERRO-CPF-PPRIE-3
                                                    PIC X(002).
           05 WR-CPF-PPRIE-3                        PIC 9(015).
           05 WR-CPF-PPRIE-3-R         REDEFINES  WR-CPF-PPRIE-3
                                                    PIC X(015).


      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 3
      * DATA NASCIMENTO PROPRIETARIO 3
           05 WR-CERRO-DNASC-PPRIE-3                PIC 9(002).
           05 WR-CERRO-DNASC-PPRIE-3-R REDEFINES  WR-CERRO-DNASC-PPRIE-3
                                                    PIC X(002).
           05 WR-DNASC-PPRIE-3                      PIC 9(006).
           05 WR-DNASC-PPRIE-3-R       REDEFINES  WR-DNASC-PPRIE-3
                                                    PIC X(006).


      * CODIGO DE ERRO TIPO PESSOA
      * TIPO PESSOA (F OU J)
           05 WR-CERRO-TPO-PSSOA                    PIC 9(002).
           05 WR-CERRO-TPO-PSSOA-R     REDEFINES  WR-CERRO-TPO-PSSOA
                                                    PIC X(002).
           05 WR-CTPO-PSSOA                         PIC X(001).


      * RESERVADO VISANET
           05 FILLER                                PIC X(003).


      * CODIGO DE ERRO TIPO TERMINAL
      * TIPO TERMINAL
           05 WR-CERRO-CTPO-TERM                    PIC 9(002).
           05 WR-CERRO-CTPO-TERM-R     REDEFINES  WR-CERRO-CTPO-TERM
                                                    PIC X(002).
           05 WR-CTPO-TERM                          PIC X(001).

      * CODIGO DE ERRO CATEGORIA ANTECIPACAO
      * CATEGORIA ANTECIPACAO
           05 WR-CERRO-CCATEG-ANTEC                 PIC 9(002).
           05 WR-CERRO-CCATEG-ANTEC-R  REDEFINES  WR-CERRO-CCATEG-ANTEC
                                                    PIC X(002).
           05 WR-CCATEG-ANTEC                       PIC X(001).


      * CODIGO DE ERRO ANTECIPACAO AUTOMATICA
      * ANTECIPACAO AUTOMATICA
           05 WR-CERRO-CANTEC-AUTOM                 PIC 9(002).
           05 WR-CERRO-CANTEC-AUTOM-R  REDEFINES  WR-CERRO-CANTEC-AUTOM
                                                    PIC X(002).
           05 WR-CANTEC-AUTOM                       PIC X(001).


      * CODIGO DE ERRO DDD TELEFONE
      * DDD TELEFONE
           05 WR-CERRO-DDD-FONE-1                   PIC 9(002).
           05 WR-CERRO-DDD-FONE-1-R    REDEFINES  WR-CERRO-DDD-FONE-1
                                                    PIC X(002).
           05 WR-CDDD-FONE-1                        PIC 9(004).
           05 WR-CDDD-FONE-1-R         REDEFINES  WR-CDDD-FONE-1
                                                    PIC X(004).


      * CODIGO DE ERRO TELEFONE
      * TELEFONE
           05 WR-CERRO-NRO-FONE-1                   PIC 9(002).
           05 WR-CERRO-NRO-FONE-1-R    REDEFINES  WR-CERRO-NRO-FONE-1
                                                    PIC X(002).
           05 WR-CFONE-PPRIE-1                      PIC 9(009).
           05 WR-CFONE-PPRIE-1-R       REDEFINES  WR-CFONE-PPRIE-1
                                                    PIC X(009).

      * CODIGO DE ERRO DDD FAX
      * DDD FAX
           05 WR-CERRO-DDD-FONE-2                   PIC 9(002).
           05 WR-CERRO-DDD-FONE-2-R    REDEFINES  WR-CERRO-DDD-FONE-2
                                                    PIC X(002).
           05 WR-CDDD-FONE-2                        PIC 9(004).
           05 WR-CDDD-FONE-2-R         REDEFINES  WR-CDDD-FONE-2
                                                    PIC X(004).

      * CODIGO DE ERRO FAX
      * FAX
           05 WR-CERRO-NRO-FONE-2                   PIC 9(002).
           05 WR-CERRO-NRO-FONE-2-R    REDEFINES  WR-CERRO-NRO-FONE-2
                                                    PIC X(002).
           05 WR-CFONE-PPRIE-2                      PIC 9(009).
           05 WR-CFONE-PPRIE-2-R       REDEFINES  WR-CFONE-PPRIE-2
                                                    PIC X(009).

      * RESERVADO VISANET
           05 FILLER                                PIC X(1193).

