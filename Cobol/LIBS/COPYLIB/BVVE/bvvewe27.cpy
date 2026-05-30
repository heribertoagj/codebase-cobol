      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWE27                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0027            *
      *----------------------------------------------------------------*
      *
            05     FILLER        REDEFINES BVVEWE00-MENSAGEM.
             07    BVVEWE27-PATH.
      *            IDENTIFICADOR DA OPERAÁ„O DE CONTESTAÁ„O.
              10   BVVEWE27-IDENTD-OP-CONTSTC               PIC X(019).
             07    BVVEWE27-MAPA.
      *            IDENTIDADE (CPF / CNPJ) DA INSTITUIÁ„O ORIGINADORA DA
      *            CONTESTAÁ„O
              10   BVVEWE27-IDENDPARTORIGDRCONTST           PIC X(014).
      *            IDENTIDADE (CPF / CNPJ) DA INSTITUIÁ„O CONTESTADA
              10   BVVEWE27-IDENDPARTCONTSTD                PIC X(014).
      *            IDENTIFICADOR CONTROLE REQUISIÁ„O SOLICITANTE
              10   BVVEWE27-IDENTDCTRLREQSOLICTE            PIC X(020).
      *            IDENTIFICADOR DO MOTIVO DA CONTESTAÁ„O
              10   BVVEWE27-INDRMOTVCONTSTC                 PIC X(002).
              10   BVVEWE27-DESCCONTSTC                     PIC X(0200).
      *            IDENTIFICADOR DA OPERAÁ„O
              10   BVVEWE27-IDENTDOP                        PIC X(019).
