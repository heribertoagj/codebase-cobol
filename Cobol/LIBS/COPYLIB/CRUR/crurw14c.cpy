      ******************************************************************
      *    BOOK : CRURW14C - INTERFACE DO COORDENADOR E FUNCIONAL      *
      *    DESCRICAO :                                                 *
      *    DATA      : 26/09/2011                                      *
      *    AUTOR     : DCSP                                            *
      *    EMPRESA   : CPM BRAXIS                                      *
      *    GRUPO     : CPM BRAXIS                                      *
      *    COMPONENTE:                                                 *
      *    TAMANHO   : 821 BYTES                                       *
      *********************** DADOS DE RETORNO *************************
      *                                                                *
      * ENTRADA:                                                       *
      * CEMPTO-RURAL         - IDENTIFICA A CLASSIFICACAO DO           *
      *                        EMPREENDIMENTO RURAL                    *
      * IEMPTO-RURAL         - NOME DO EMPREENDIMENTO RURAL            *
      * REMPTO-RURAL         - DETALHE DO EMPREENDIMENTO RURAL         *
      * DINIC-VGCIA-EMPTO    - DATA INICIADA A VALIDADE DO             *
      *                        EMPREENDIMENTO RURAL                    *
      * DFIM-VGCIA-EMPTO     - DATA TERMINO DA VALIDADE DO             *
      *                        EMPREENDIMENTO RURAL                    *
      * CIND-EMPTO-IDPDT     - INDICA SE EMPREENDIMENTO RURAL DEVEM SER*
      *                        QUITADO PARA AQUISICAO DE NOVOS         *
      *                        FINANCIAMENTOS                          *
      * CSIT-REG-RURAL       - CODIGO QUE IDENTIFICA EMPREENDIMENTO DE *
      *                        CREDITO RURAL E RESULTADO DE UMA        *
      *                        TRANSFORMACAO DE OUTRO EMPREENDIMENTO   *
      * CUSUAR-INCL          - CODIGO USUARIO INCLUSAO                 *
      * HINCL-REG            - HORARIO DE INCLUSAO                     *
      * CUSUAR-MANUT         - CODIGO USUARIO MANUTENCAO               *
      * HMANUT-REG           - HORARIO DE MANUTENCAO                   *
      * CEMPTO-RURAL-TRFMD   - INDICA DATA/HORA EM QUE UM DETERMINADO  *
      *                        REGISTRO SOFREU ALTERACAO               *
      * SAIDA:                                                         *
      * CEMPTO-RURAL         - IDENTIFICA A CLASSIFICACAO DO           *
      *                        EMPREENDIMENTO                          *
      * CUND-MEDD            - IDENTIFICA PADRAO FIXADO DE VALORES PARA*
      *                        DETERMINA GRANDEZA DA MESMA ESPECIE     *
      * IEMPTO-RURAL         - NOME DO EMPREENDIMENTO RURAL            *
      * REMPTO-RURAL         - DETALHE DO EMPREENDIMENTO RURAL         *
      * DINIC-VGCIA-EMPTO    - DATA INICIADA A VALIDADE DO             *
      *                        EMPREENDIMENTO RURAL                    *
      * DFIM-VGCIA-EMPTO     - DATA TERMINO DA VALIDADE DO             *
      *                        EMPREENDIMENTO RURAL                    *
      * CIND-EMPTO-IDPDT     - INDICA SE EMPREENDIMENTO RURAL DEVEM SER*
      *                        QUITADO PARA AQUISICAO DE NOVOS         *
      *                        FINANCIAMENTOS                          *
      * CSIT-REG-RURAL       - CODIGO QUE IDENTIFICA EMPREENDIMENTO DE *
      *                        CREDITO RURAL E RESULTADO DE UMA        *
      *                        TRANSFORMACAO DE OUTRO EMPREENDIMENTO   *
      * CUSUAR-INCL          - CODIGO USUARIO INCLUSAO                 *
      * HINCL-REG            - HORARIO DE INCLUSAO                     *
      * CUSUAR-MANUT         - CODIGO USUARIO MANUTENCAO               *
      * HMANUT-REG           - HORARIO DE MANUTENCAO                   *
      * CEMPTO-RURAL-TRFMD   - INDICA DATA/HORA EM QUE UM DETERMINADO  *
      *                        REGISTRO SOFREU ALTERACAO               *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      * 23/02/2016 LUCAS BARRETO     INCLUSAO CTEMPO-VIDA-CULTR        *
      ******************************************************************
           05 CRURW14C-HEADER.
             10 CRURW14C-COD-LAYOUT    PIC X(08)   VALUE 'CRURW14C'.
             10 CRURW14C-TAM-LAYOUT    PIC 9(05)   VALUE 821.
           05 CRURW14C-REGISTRO.
             10 CRURW14C-BLOCO-ENTRADA.
               15 CRURW14C-E-CEMPTO-RURAL         PIC 9(006).
               15 CRURW14C-E-CUND-MEDD            PIC 9(003).
               15 CRURW14C-E-IEMPTO-RURAL         PIC X(040).
               15 CRURW14C-E-REMPTO-RURAL         PIC X(256).
               15 CRURW14C-E-DINIC-VGCIA-EMPTO    PIC X(010).
               15 CRURW14C-E-DFIM-VGCIA-EMPTO     PIC X(010).
               15 CRURW14C-E-CIND-EMPTO-IDPDT     PIC X(001).
               15 CRURW14C-E-CEMPTO-RURAL-TRFMD   PIC X(001).
               15 CRURW14C-E-CSIT-REG-RURAL       PIC X(001).
               15 CRURW14C-E-CUSUAR-INCL          PIC X(009).
               15 CRURW14C-E-HINCL-REG            PIC X(026).
               15 CRURW14C-E-CUSUAR-MANUT         PIC X(009).
               15 CRURW14C-E-HMANUT-REG           PIC X(026).
               15 CRURW14C-E-CPRODT-RURAL-BACEN   PIC 9(004).
               15 CRURW14C-E-CVARDE-EMPTO-BACEN   PIC 9(001).
               15 CRURW14C-E-CTEMPO-VIDA-CULTR    PIC 9(001).
             10 CRURW14C-BLOCO-SAIDA.
               15 CRURW14C-S-CEMPTO-RURAL         PIC 9(006).
               15 CRURW14C-S-CUND-MEDD            PIC 9(003).
               15 CRURW14C-S-IEMPTO-RURAL         PIC X(040).
               15 CRURW14C-S-REMPTO-RURAL         PIC X(256).
               15 CRURW14C-S-DINIC-VGCIA-EMPTO    PIC X(010).
               15 CRURW14C-S-DFIM-VGCIA-EMPTO     PIC X(010).
               15 CRURW14C-S-CIND-EMPTO-IDPDT     PIC X(001).
               15 CRURW14C-S-CEMPTO-RURAL-TRFMD   PIC X(001).
               15 CRURW14C-S-CSIT-REG-RURAL       PIC X(001).
               15 CRURW14C-S-CUSUAR-INCL          PIC X(009).
               15 CRURW14C-S-HINCL-REG            PIC X(026).
               15 CRURW14C-S-CUSUAR-MANUT         PIC X(009).
               15 CRURW14C-S-HMANUT-REG           PIC X(026).
               15 CRURW14C-S-CPRODT-RURAL-BACEN   PIC 9(004).
               15 CRURW14C-S-CVARDE-EMPTO-BACEN   PIC 9(001).
               15 CRURW14C-S-CTEMPO-VIDA-CULTR    PIC 9(001).
