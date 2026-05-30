      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWX27                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0027            *
      *----------------------------------------------------------------*
      *
            05     FILLER        REDEFINES BVVEWE00-MENSAGEM.
             07    BVVEWX27-PATH.
      *            IDENTIFICADOR DA OPERAÁ„O DE CONTESTAÁ„O.
              10   BVVEWX27-IDENTD-OP-CONTSTC               PIC X(019).
             07    BVVEWX27-MAPA.
      *            IDENTIDADE (CPF / CNPJ) DA INSTITUIÁ„O ORIGINADORA DA
      *            CONTESTAÁ„O
              10   BVVEWX27-IDENDPARTORIGDRCONTST           PIC X(014).
      *            IDENTIDADE (CPF / CNPJ) DA INSTITUIÁ„O CONTESTADA
              10   BVVEWX27-IDENDPARTCONTSTD                PIC X(014).
      *            IDENTIFICADOR CONTROLE REQUISIÁ„O SOLICITANTE
              10   BVVEWX27-IDENTDCTRLREQSOLICTE            PIC X(020).
      *            IDENTIFICADOR DO MOTIVO DA CONTESTAÁ„O
              10   BVVEWX27-INDRMOTVCONTSTC                 PIC X(002).
              10   BVVEWX27-DESCCONTSTC                     PIC X(0200).
      *            IDENTIFICADOR CONTROLE OPT-IN
              10   BVVEWX27-IDENTDCTRLOPTIN                 PIC X(019).
