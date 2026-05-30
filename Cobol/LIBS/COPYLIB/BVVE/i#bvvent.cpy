      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEA816                     *
      *            ENVIA SOLICITACAO DE DESCONSTITUICAO - RRC0025      *
      *----------------------------------------------------------------*
      *
       01 BVVENT-COMMAREA.
          03  BVVENT-NSOLTC-DESCONST       PIC 9(009).
          03  BVVENT-SITU-RET-ENVIAR       PIC 9(002).
          03  BVVENT-JUSTIFICATIVA         PIC X(250).
          03  BVVENT-SAIDA.
              05 BVVENT-COD-RETORNO        PIC 9(005).
              05 BVVENT-MENSAGEM           PIC X(079).

      *   BVVENT-SITU-RET-ENVIAR: 1 EM PROCESSAMENTO (ACEITE PARCIAL)
      *                           2 ACEITO
      *                           3 RECUSADO
      *   (CONFORME MANUAL CIP)

