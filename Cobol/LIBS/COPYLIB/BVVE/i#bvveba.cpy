      *****************************************************************
      *   BVVE - AFILIACAO ELETRONICA DE ESTABELECIMENTOS             *
      *---------------------------------------------------------------*
      *   I#BVVEBA - MOVIMENTO DE ENVIO VISANET                       *
      *   TAMANHO  - 1920 BYTES                                       *
      *****************************************************************
      *
       01  BVVEBA-REGISTRO.
      * IDENTIFICACAO DE ARQUIVO / ESTABELECIMENTO
           05 BA-MENSAGEM             PIC 9(003).
           05 BA-IDENT-SOLCT.
              10 BA-NUM-INSTT-FINAN   PIC 9(004).
              10 BA-NUM-IDENT-CHAMD   PIC X(004).
      * RESERVADO VISANET
           05 FILLER                  PIC X(019).
      * CODIGO DE ERRO GENERICO
           05 BA-CERRO                PIC 9(002).
      * CODIGO DE ERRO BANCO
      * BANCO
           05 BA-CERRO-CBANCO         PIC 9(002).
           05 BA-CBANCO               PIC 9(003).
      * NUMERO DO ESTABELECIMENTO
           05 BA-NUM-ESTBL            PIC 9(010).
      * CODIGO DE ERRO ESTABELECIMENTO
      * ESTABELECIMENTO
           05 BA-CERRO-IESTBL         PIC 9(002).
           05 BA-IESTBL               PIC X(032).
      * CODIGO DE ERRO ENDERECO CORRESPONDENCIA
      * ENDERECO CORRESPONDENCIA
           05 BA-CERRO-ENDER-1        PIC 9(002).
           05 BA-EAFLIA-1             PIC X(064).
      * CODIGO DE ERRO CIDADE CORRESPONDENCIA
      * CIDADE CORRESPONDENCIA
           05 BA-CERRO-CIDDE-1        PIC 9(002).
           05 BA-IMUN-1               PIC X(028).
      * CODIGO DE ERRO ESTADO CORRESPONDENCIA
      * ESTADO CORRESPONDENCIA
           05 BA-CERRO-CSGL-UF-1      PIC 9(002).
           05 BA-CSGL-UF-1            PIC X(002).
      * CODIGO DE ERRO CEP CORRESPONDENCIA
      * CEP CORRESPONDENCIA
           05 BA-CERRO-CCEP-1         PIC 9(002).
           05 BA-CCEP-1               PIC 9(008).
      * CODIGO DE ERRO CNPJ/CPF
      * CNPJ/CPF
           05 BA-CERRO-CCNPJ-CPF      PIC 9(002).
           05 BA-CCNPJ-CPF            PIC 9(015).
      * CODIGO DE ERRO INSCRICAO ESTADUAL
      * INSCRICAO ESTADUAL
           05 BA-CERRO-CINSCR-EST     PIC 9(002).
           05 BA-CINSCR-EST           PIC 9(015).
      * CODIGO DE ERRO NOME FANTASIA
      * NOME FANTASIA
           05 BA-CERRO-IFANTS         PIC 9(002).
           05 BA-IFANTS-ESTBL         PIC X(032).
      * CODIGO DE ERRO ENDERECO FISCAL
      * ENDERECO FISCAL
           05 BA-CERRO-ENDER-2        PIC 9(002).
           05 BA-EAFLIA-2             PIC X(064).
      * CODIGO DE ERRO CIDADE FISCAL
      * CIDADE FISCAL
           05 BA-CERRO-CIDDE-2        PIC 9(002).
           05 BA-IMUN-2               PIC X(028).
      * CODIGO DE ERRO ESTADO FISCAL
      * ESTADO FISCAL
           05 BA-CERRO-CSGL-UF-2      PIC 9(002).
           05 BA-CSGL-UF-2            PIC X(002).
      * CODIGO DE ERRO CEP FISCAL
      * CEP FISCAL
           05 BA-CERRO-CCEP-2         PIC 9(002).
           05 BA-CCEP-2               PIC 9(008).
      * CODIGO DE ERRO PLAQUETA
      * PLAQUETA
           05 BA-CERRO-IPLAQT         PIC 9(002).
           05 BA-IPLAQT               PIC X(022).
      * CODIGO DE ERRO PESSOA CONTATO
      * PESSOA CONTATO
           05 BA-CERRO-CNTAT          PIC 9(002).
           05 BA-ICNTAT-ESTBL         PIC X(032).
      * RESERVA CIELO
           05 FILLER                  PIC X(032).
           05 BA-ZEROS-1              PIC 9(002).
           05 BA-ZEROS-2              PIC 9(010).
      * CODIGO DE ERRO RAMO ATIVIDADE
      * RAMO ATIVIDADE
           05 BA-CERRO-RAMO-ATVDD     PIC 9(002).
           05 BA-CTPO-ATVDD           PIC 9(005).
      * CODIGO DE ERRO AGENCIA
      * AGENCIA
           05 BA-CERRO-CAG            PIC 9(002).
           05 BA-CAG-BCARIA           PIC 9(005).
      * CODIGO DE ERRO CONTA CORRENTE
      * CONTA CORRENTE
           05 BA-CERRO-CORR           PIC 9(002).
           05 BA-CCTA-BCARIA          PIC X(014).
      * CODIGO DE ERRO NOME PROPRIETARIO 1
      * NOME PROPRIETARIO 1
           05 BA-CERRO-NOME-PPRIE-1   PIC 9(002).
           05 BA-IPPRIE-1             PIC X(032).
      * CODIGO DE ERRO CPF PROPRIETARIO 1
      * CPF PROPRIETARIO 1
           05 BA-CERRO-CPF-PPRIE-1    PIC 9(002).
           05 BA-CPF-PPRIE-1          PIC 9(015).
      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 1
      * DATA NASCIMENTO PROPRIETARIO 1
           05 BA-CERRO-DNASC-PPRIE-1  PIC 9(002).
           05 BA-DNASC-PPRIE-1        PIC 9(006).
      * CODIGO DE ERRO NOME PROPRIETARIO 2
      * NOME PROPRIETARIO 2
           05 BA-CERRO-NOME-PPRIE-2   PIC 9(002).
           05 BA-IPPRIE-2             PIC X(032).
      * CODIGO DE ERRO CPF PROPRIETARIO 2
      * CPF PROPRIETARIO 2
           05 BA-CERRO-CPF-PPRIE-2    PIC 9(002).
           05 BA-CPF-PPRIE-2          PIC 9(015).
      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 2
      * DATA NASCIMENTO PROPRIETARIO 2
           05 BA-CERRO-DNASC-PPRIE-2  PIC 9(002).
           05 BA-DNASC-PPRIE-2        PIC 9(006).
      * CODIGO DE ERRO NOME PROPRIETARIO 3
      * NOME PROPRIETARIO 3
           05 BA-CERRO-NOME-PPRIE-3   PIC 9(002).
           05 BA-IPPRIE-3             PIC X(032).
      * CODIGO DE ERRO CPF PROPRIETARIO 3
      * CPF PROPRIETARIO 3
           05 BA-CERRO-CPF-PPRIE-3    PIC 9(002).
           05 BA-CPF-PPRIE-3          PIC 9(015).
      * CODIGO DE ERRO DATA NASCIMENTO PROPRIETARIO 3
      * DATA NASCIMENTO PROPRIETARIO 3
           05 BA-CERRO-DNASC-PPRIE-3  PIC 9(002).
           05 BA-DNASC-PPRIE-3        PIC 9(006).
      * CODIGO DE ERRO TIPO PESSOA
      * TIPO PESSOA (F OU J)
           05 BA-CERRO-TPO-PSSOA      PIC 9(002).
           05 BA-CTPO-PSSOA           PIC X(001).
      * RESERVADO VISANET
           05 FILLER                  PIC X(003).
      * CODIGO DE ERRO TIPO TERMINAL
      * TIPO TERMINAL
           05 BA-CERRO-CTPO-TERM      PIC 9(002).
           05 BA-CTPO-TERM            PIC X(001).

JR         05 FILLER                  PIC X(006).

      * CODIGO DE ERRO DDD TELEFONE
      * DDD TELEFONE
           05 BA-CERRO-DDD-FONE-1     PIC 9(002).
           05 BA-CDDD-FONE-1          PIC 9(004).
      * CODIGO DE ERRO TELEFONE
      * TELEFONE
           05 BA-CERRO-NRO-FONE-1     PIC 9(002).
           05 BA-CFONE-PPRIE-1        PIC 9(009).
      * CODIGO DE ERRO DDD FAX
      * DDD FAX
           05 BA-CERRO-DDD-FONE-2     PIC 9(002).
           05 BA-CDDD-FONE-2          PIC 9(004).
      * CODIGO DE ERRO FAX
      * FAX
           05 BA-CERRO-NRO-FONE-2     PIC 9(002).
           05 BA-CFONE-PPRIE-2        PIC 9(009).

      * RESERVADO VISANET
JR         05 FILLER                  PIC X(455).

      * CODIGO DE ERRO CATEGORIA ANTECIPACAO
      * CATEGORIA ANTECIPACAO
           05 BA-CERRO-CCATEG-ANTEC   PIC 9(002).
           05 BA-CCATEG-ANTEC         PIC X(001).
      * CODIGO DE ERRO ANTECIPACAO AUTOMATICA
      * ANTECIPACAO AUTOMATICA
           05 BA-CERRO-CANTEC-AUTOM   PIC 9(002).
           05 BA-CANTEC-AUTOM         PIC X(001).

      * RESERVADO VISANET
JR         05 FILLER                  PIC X(732).

