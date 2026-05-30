      *****************************************************************
      *   BVVE - AFILIACAO ELETRONICA DE ESTABELECIMENTOS             *
      *---------------------------------------------------------------*
      *   I#BVVEBG - MOVIMENTO DE RETORNO VISANET                     *
      *   TAMANHO  - 1920 BYTES                                       *
      *****************************************************************
      *
       01  BVVEBG-REGISTRO.
      * IDENTIFICACAO DE ARQUIVO / ESTABELECIMENTO
           05 BG-MENSAGEM                          PIC 9(003).
           05 BG-MENSAGEM-R            REDEFINES  BG-MENSAGEM
                                                   PIC X(003).


           05 BG-IDENT-SOLCT.
              10 BG-NUM-INSTT-FINAN                PIC 9(004).
              10 BG-NUM-INSTT-FINAN-R  REDEFINES  BG-NUM-INSTT-FINAN
                                                   PIC X(004).
              10 BG-NUM-IDENT-CHAMD                PIC X(004).


      * RESERVADO VISANET
           05 FILLER                               PIC X(019).


      * CODIGO DE ERRO GENERICO
           05 BG-CERRO                             PIC 9(002).
           05 BG-CERRO-R               REDEFINES  BG-CERRO
                                                   PIC X(002).

      * CODIGO DE ERRO BANCO
      * BANCO
           05 BG-CERRO-CBANCO                      PIC 9(002).
           05 BG-CERRO-CBANCO-R        REDEFINES  BG-CERRO-CBANCO
                                                   PIC X(002).
           05 BG-CBANCO                            PIC 9(003).
           05 BG-CBANCO-R              REDEFINES  BG-CBANCO
                                                   PIC X(003).


      * NUMERO DO ESTABELECIMENTO
           05 BG-NUM-ESTBL                         PIC 9(010).
           05 BG-NUM-ESTBL-R           REDEFINES  BG-NUM-ESTBL
                                                   PIC X(010).


      * CODIGO DE ERRO ESTABELECIMENTO
      * ESTABELECIMENTO
           05 BG-CERRO-IESTBL                      PIC 9(002).
           05 BG-CERRO-IESTBL-R        REDEFINES  BG-CERRO-IESTBL
                                                   PIC X(002).
           05 BG-IESTBL                            PIC X(032).


      * CODIGO DE ERRO ENDERECO CORRESPONDENCIA
      * ENDERECO CORRESPONDENCIA
EMER       05 BG-CERRO-ENDER-1                    PIC 9(002).
EMER       05 BG-CERRO-ENDER-1-R      REDEFINES  BG-CERRO-ENDER-1
EMER                                              PIC X(002).
EMER       05 BG-EAFLIA-1                         PIC X(064).


      * CODIGO DE ERRO CIDADE CORRESPONDENCIA
      * CIDADE CORRESPONDENCIA
           05 BG-CERRO-CIDDE-1                     PIC 9(002).
           05 BG-CERRO-CIDDE-1-R       REDEFINES  BG-CERRO-CIDDE-1
                                                   PIC X(002).
           05 BG-IMUN-1                            PIC X(028).


      * CODIGO DE ERRO ESTADO CORRESPONDENCIA
      * ESTADO CORRESPONDENCIA
           05 BG-CERRO-CSGL-UF-1                   PIC 9(002).
           05 BG-CERRO-CSGL-UF-1-R     REDEFINES  BG-CERRO-CSGL-UF-1
                                                    PIC X(002).
           05 BG-CSGL-UF-1                         PIC X(002).


      * CODIGO DE ERRO CEP CORRESPONDENCIA
      * CEP CORRESPONDENCIA
           05 BG-CERRO-CCEP-1                      PIC 9(002).
           05 BG-CERRO-CCEP-1-R        REDEFINES  BG-CERRO-CCEP-1
                                                   PIC X(002).
           05 BG-CCEP-1                            PIC 9(008).
           05 BG-CCEP-1-R              REDEFINES  BG-CCEP-1
                                                    PIC X(008).


      * CODIGO DE ERRO CNPJ/CPF
      * CNPJ/CPF
           05 BG-CERRO-CCNPJ-CPF                   PIC 9(002).
           05 BG-CERRO-CCNPJ-CPF-R     REDEFINES  BG-CERRO-CCNPJ-CPF
                                                   PIC X(002).
           05 BG-CCNPJ-CPF                         PIC 9(015).
           05 BG-CCNPJ-CPF-R           REDEFINES  BG-CCNPJ-CPF
                                                    PIC X(015).


      * CODIGO DE ERRO INSCRICAO ESTADUAL
      * INSCRICAO ESTADUAL
           05 BG-CERRO-CINSCR-EST                   PIC 9(002).
           05 BG-CERRO-CINSCR-EST-R    REDEFINES  BG-CERRO-CINSCR-EST
                                                    PIC X(002).
           05 BG-CINSCR-EST                         PIC 9(015).
           05 BG-CINSCR-EST-R          REDEFINES  BG-CINSCR-EST
                                                    PIC X(015).


      * CODIGO DE ERRO NOME FANTASIA
      * NOME FANTASIA
           05 BG-CERRO-IFANTS                       PIC 9(002).
           05 BG-CERRO-IFANTS-R        REDEFINES  BG-CERRO-IFANTS
                                                    PIC X(002).
           05 BG-IFANTS-ESTBL                       PIC X(032).


      * CODIGO DE ERRO ENDERECO FISCAL
      * ENDERECO FISCAL
           05 BG-CERRO-ENDER-2                      PIC 9(002).
           05 BG-CERRO-ENDER-2-R       REDEFINES  BG-CERRO-ENDER-2
                                                    PIC X(002).
           05 BG-EAFLIA-2                           PIC X(064).


      * CODIGO DE ERRO CIDADE FISCAL
      * CIDADE FISCAL
           05 BG-CERRO-CIDDE-2                      PIC 9(002).
           05 BG-CERRO-CIDDE-2-R       REDEFINES  BG-CERRO-CIDDE-2
                                                    PIC X(002).
           05 BG-IMUN-2                             PIC X(028).


      * CODIGO DE ERRO ESTADO FISCAL
      * ESTADO FISCAL
           05 BG-CERRO-CSGL-UF-2                    PIC 9(002).
           05 BG-CERRO-CSGL-UF-2-R     REDEFINES  BG-CERRO-CSGL-UF-2
                                                    PIC X(002).
           05 BG-CSGL-UF-2                          PIC X(002).


      * CODIGO DE ERRO CEP FISCAL
      * CEP FISCAL
           05 BG-CERRO-CCEP-2                       PIC 9(002).
           05 BG-CERRO-CCEP-2-R        REDEFINES  BG-CERRO-CCEP-2
                                                    PIC X(002).
           05 BG-CCEP-2                             PIC 9(008).
           05 BG-CCEP-2-R              REDEFINES  BG-CCEP-2
                                                    PIC X(008).


      * CODIGO DE ERRO PLAQUETA
      * PLAQUETA
           05 BG-CERRO-IPLAQT                       PIC 9(002).
           05 BG-CERRO-IPLAQT-R        REDEFINES  BG-CERRO-IPLAQT
                                                    PIC X(002).
           05 BG-IPLAQT                             PIC X(022).


      * CODIGO DE ERRO PESSOA CONTATO
      * PESSOA CONTATO
           05 BG-CERRO-CNTAT                        PIC 9(002).
           05 BG-CERRO-CNTAT-R         REDEFINES  BG-CERRO-CNTAT
                                                    PIC X(002).
           05 BG-ICNTAT-ESTBL                       PIC X(032).

      * AREA RESERVADA
           05 FILLER                                PIC X(032).
           05 BG-ZEROS-1                            PIC 9(002).
           05 BG-ZEROS-2                            PIC 9(010).


      * CODIGO DE ERRO RAMO ATIVIDADE
      * RAMO ATIVIDADE
           05 BG-CERRO-RAMO-ATVDD                   PIC 9(002).
           05 BG-CERRO-RAMO-ATVDD-R    REDEFINES  BG-CERRO-RAMO-ATVDD
                                                    PIC X(002).
           05 BG-CTPO-ATVDD                         PIC 9(005).
           05 BG-CTPO-ATVDD-R          REDEFINES  BG-CTPO-ATVDD
                                                    PIC X(005).


      * CODIGO DE ERRO AGENCIA
      * AGENCIA
           05 BG-CERRO-CAG                          PIC 9(002).
           05 BG-CERRO-CAG-R           REDEFINES  BG-CERRO-CAG
                                                    PIC X(002).
           05 BG-CAG-BCARIA                         PIC 9(005).
           05 BG-CAG-BCARIA-R          REDEFINES  BG-CAG-BCARIA
                                                    PIC X(005).


      * CODIGO DE ERRO CONTA CORRENTE
      * CONTA CORRENTE
           05 BG-CERRO-CORR                         PIC 9(002).
           05 BG-CERRO-CORR-R          REDEFINES  BG-CERRO-CORR
                                                    PIC X(002).
EMER       05 BG-CCTA-BCARIA                        PIC 9(014).
           05 BG-CCTA-BCARIA-R         REDEFINES  BG-CCTA-BCARIA
                                                    PIC X(014).


      * CODIGO DE ERRO NOME PROPRIETARIO 1
      * NOME PROPRIETARIO 1
           05 BG-CERRO-NOME-PPRIE-1                 PIC 9(002).
           05 BG-CERRO-NOME-PPRIE-1-R  REDEFINES  BG-CERRO-NOME-PPRIE-1
                                                    PIC X(002).
           05 BG-IPPRIE-1                           PIC X(032).


      * CODIGO DE ERRO CPF PROPRIETARIO 1
      * CPF PROPRIETARIO 1
           05 BG-CERRO-CPF-PPRIE-1                  PIC 9(002).
           05 BG-CERRO-CPF-PPRIE-1-R   REDEFINES  BG-CERRO-CPF-PPRIE-1
                                                    PIC X(002).
           05 BG-CPF-PPRIE-1                        PIC 9(015).
           05 BG-CPF-PPRIE-1-R         REDEFINES  BG-CPF-PPRIE-1
                                                    PIC X(015).


      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 1
      * DATA NASCIMENTO PROPRIETARIO 1
           05 BG-CERRO-DNASC-PPRIE                  PIC 9(002).
           05 BG-CERRO-DNASC-PPRIE-R   REDEFINES  BG-CERRO-DNASC-PPRIE
                                                    PIC X(002).
           05 BG-DNASC-PPRIE                        PIC 9(006).
           05 BG-DNASC-PPRIE-R         REDEFINES  BG-DNASC-PPRIE
                                                    PIC X(006).


      * CODIGO DE ERRO NOME PROPRIETARIO 2
      * NOME PROPRIETARIO 2
           05 BG-CERRO-NOME-PPRIE-2                 PIC 9(002).
           05 BG-CERRO-NOME-PPRIE-2-R  REDEFINES  BG-CERRO-NOME-PPRIE-2
                                                    PIC X(002).
           05 BG-IPPRIE-2                           PIC X(032).


      * CODIGO DE ERRO CPF PROPRIETARIO 2
      * CPF PROPRIETARIO 2
           05 BG-CERRO-CPF-PPRIE-2                  PIC 9(002).
           05 BG-CERRO-CPF-PPRIE-2-R   REDEFINES  BG-CERRO-CPF-PPRIE-2
                                                    PIC X(002).
           05 BG-CPF-PPRIE-2                        PIC 9(015).
           05 BG-CPF-PPRIE-2-R         REDEFINES  BG-CPF-PPRIE-2
                                                    PIC 9(015).


      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 2
      * DATA NASCIMENTO PROPRIETARIO 2
           05 BG-CERRO-DNASC-PPRIE-2                PIC 9(002).
           05 BG-CERRO-DNASC-PPRIE-2-R REDEFINES  BG-CERRO-DNASC-PPRIE-2
                                                    PIC X(002).
           05 BG-DNASC-PPRIE-2                      PIC 9(006).
           05 BG-DNASC-PPRIE-2-R       REDEFINES  BG-DNASC-PPRIE-2
                                                    PIC X(006).


      * CODIGO DE ERRO NOME PROPRIETARIO 3
      * NOME PROPRIETARIO 3
           05 BG-CERRO-NOME-PPRIE-3                 PIC 9(002).
           05 BG-CERRO-NOME-PPRIE-3-R  REDEFINES  BG-CERRO-NOME-PPRIE-3
                                                    PIC X(002).
           05 BG-IPPRIE-3                           PIC X(032).


      * CODIGO DE ERRO CPF PROPRIETARIO 3
      * CPF PROPRIETARIO 3
           05 BG-CERRO-CPF-PPRIE-3                  PIC 9(002).
           05 BG-CERRO-CPF-PPRIE-3-R   REDEFINES  BG-CERRO-CPF-PPRIE-3
                                                    PIC X(002).
           05 BG-CPF-PPRIE-3                        PIC 9(015).
           05 BG-CPF-PPRIE-3-R         REDEFINES  BG-CPF-PPRIE-3
                                                    PIC X(015).


      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 3
      * DATA NASCIMENTO PROPRIETARIO 3
           05 BG-CERRO-DNASC-PPRIE-3                PIC 9(002).
           05 BG-CERRO-DNASC-PPRIE-3-R REDEFINES  BG-CERRO-DNASC-PPRIE-3
                                                    PIC X(002).
           05 BG-DNASC-PPRIE-3                      PIC 9(006).
           05 BG-DNASC-PPRIE-3-R       REDEFINES  BG-DNASC-PPRIE-3
                                                    PIC X(006).


      * CODIGO DE ERRO TIPO PESSOA
      * TIPO PESSOA (F OU J)
           05 BG-CERRO-TPO-PSSOA                    PIC 9(002).
           05 BG-CERRO-TPO-PSSOA-R     REDEFINES  BG-CERRO-TPO-PSSOA
                                                    PIC X(002).
           05 BG-CTPO-PSSOA                         PIC X(001).


      * RESERVADO VISANET
           05 FILLER                                PIC X(003).


      * CODIGO DE ERRO TIPO TERMINAL
      * TIPO TERMINAL
           05 BG-CERRO-CTPO-TERM                    PIC 9(002).
           05 BG-CERRO-CTPO-TERM-R     REDEFINES  BG-CERRO-CTPO-TERM
                                                    PIC X(002).
           05 BG-CTPO-TERM                          PIC X(001).


JR         05 FILLER                                PIC X(006).

      * CODIGO DE ERRO DDD TELEFONE
      * DDD TELEFONE
           05 BG-CERRO-DDD-FONE-1                   PIC 9(002).
           05 BG-CERRO-DDD-FONE-1-R    REDEFINES  BG-CERRO-DDD-FONE-1
                                                    PIC X(002).
           05 BG-CDDD-FONE-1                        PIC 9(004).
           05 BG-CDDD-FONE-1-R         REDEFINES  BG-CDDD-FONE-1
                                                    PIC X(004).

      * CODIGO DE ERRO TELEFONE
      * TELEFONE
           05 BG-CERRO-NRO-FONE-1                   PIC 9(002).
           05 BG-CERRO-NRO-FONE-1-R    REDEFINES  BG-CERRO-NRO-FONE-1
                                                    PIC X(002).
           05 BG-CFONE-PPRIE-1                      PIC 9(009).
           05 BG-CFONE-PPRIE-1-R       REDEFINES  BG-CFONE-PPRIE-1
                                                    PIC X(009).

      * CODIGO DE ERRO DDD FAX
      * DDD FAX
           05 BG-CERRO-DDD-FONE-2                   PIC 9(002).
           05 BG-CERRO-DDD-FONE-2-R    REDEFINES  BG-CERRO-DDD-FONE-2
                                                    PIC X(002).
           05 BG-CDDD-FONE-2                        PIC 9(004).
           05 BG-CDDD-FONE-2-R         REDEFINES  BG-CDDD-FONE-2
                                                    PIC X(004).

      * CODIGO DE ERRO FAX
      * FAX
           05 BG-CERRO-NRO-FONE-2                   PIC 9(002).
           05 BG-CERRO-NRO-FONE-2-R    REDEFINES  BG-CERRO-NRO-FONE-2
                                                    PIC X(002).
           05 BG-CFONE-PPRIE-2                      PIC 9(009).
           05 BG-CFONE-PPRIE-2-R       REDEFINES  BG-CFONE-PPRIE-2
                                                    PIC X(009).

JR         05 FILLER                                PIC X(455).

      * CODIGO DE ERRO CATEGORIA ANTECIPACAO
      * CATEGORIA ANTECIPACAO
           05 BG-CERRO-CCATEG-ANTEC                 PIC 9(002).
           05 BG-CERRO-CCATEG-ANTEC-R  REDEFINES  BG-CERRO-CCATEG-ANTEC
                                                    PIC X(002).
           05 BG-CCATEG-ANTEC                       PIC X(001).


      * CODIGO DE ERRO ANTECIPACAO AUTOMATICA
      * ANTECIPACAO AUTOMATICA
           05 BG-CERRO-CANTEC-AUTOM                 PIC 9(002).
           05 BG-CERRO-CANTEC-AUTOM-R  REDEFINES  BG-CERRO-CANTEC-AUTOM
                                                    PIC X(002).
           05 BG-CANTEC-AUTOM                       PIC X(001).

      * RESERVADO VISANET
JR         05 FILLER                                PIC X(732).

