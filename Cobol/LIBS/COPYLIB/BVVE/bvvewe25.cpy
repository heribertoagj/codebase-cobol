      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWE25                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0025            *
      *----------------------------------------------------------------*
      *
            05     FILLER      REDEFINES BVVEWE00-MENSAGEM.
      *            IDENTIFICADOR DA OPERAÁ„O RECUSADA.
             07    BVVEWE25-IDENTD-OP                       PIC X(019).
             07    BVVEWE25-MAPA.
      *            IDENTIFICADOR NEGOCIAÁ„O RECEBÌVEL.
              10   BVVEWE25-IDENTDNEGCRECBVL                PIC X(019).
      *            CNPJ (COMPLETO OU BASE) OU CPF DO TITULAR.
              10   BVVEWE25-CNPJCPFTITLAR                   PIC X(014).
      *            IDENTIFICADOR DA OPERAÁ„O DESCONSITUIÁ„O DA
      *            NEGOCIAÁ„O DE RECEBÌVEL.
              10   BVVEWE25-IDENTDOPDESTCNEGCREC            PIC X(019).
      *            SITUAÁ„O PEDIDO DESCONSTITUIÁ„O NEGOCIAÁ„O DE RECEBÌV
              10   BVVEWE25-SITPEDESCSTCNEGRECBVL           PIC 9(002).
