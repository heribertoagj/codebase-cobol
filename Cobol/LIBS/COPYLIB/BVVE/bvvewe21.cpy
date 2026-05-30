      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWE21                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0021            *
      *----------------------------------------------------------------*
      *
            05     FILLER      REDEFINES BVVEWE00-MENSAGEM.
      *            TIPO DE COMUNICACAO:
      *            1 - CONSULTAR OPERACAO
      *            2 - CONSULTAR UNIDADES RECEBIVEIS
      *            3 - CONSULTAR UNIDADES RECEBIVEIS A CONSTITUIR
             07    BVVEWE21-TP-COMU                         PIC 9(001).
      *            IDENTIFICADOR DA OPERAÁ„O PARA CONSULTA UNICA.
             07    BVVEWE21-IDENTD-OP                       PIC X(019).
             07    BVVEWE21-MAPA.
      *            CNPJ (COMPLETO OU BASE) OU CPF USU·RIO FINAL RECEBEDO
              10   BVVEWE21-CNPJCPFUSUFINRECBDR             PIC X(014).
      *            CNPJ (COMPLETO OU BASE) OU CPF DO TITULAR DO RECEBÌVE
              10   BVVEWE21-CNPJCPFTITLAR                   PIC X(014).
      *            NAO PREENCHER
              10   BVVEWE21-NUMEROPAGINA-X.
                15 BVVEWE21-NUMEROPAGINA                    PIC 9(003).
      *            NAO PREENCHER
              10   BVVEWE21-TAMANHOPAGINA-X.
                15 BVVEWE21-TAMANHOPAGINA                   PIC 9(005).
