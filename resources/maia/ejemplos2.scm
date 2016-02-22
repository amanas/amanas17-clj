[ ;;principio de la lista de ejemplos
 [ ;;definicion de los atributos
  [:perspectiva [:nublado :soleado :lluvioso :niebla :diluvia]]
  [:temperatura :numerico]
  [:humedad :numerico]
  [:viento [:fuerte :brisa :no]]
  [:animo [:contento :normal :triste]]
  [:estres [:relajado :normal :estresado]]
  [:dinero [:solvente :ajustado :insuficiente]]
  [:clase [:+ :-]]] ;;fin definicion de atributos
 ;;Ejemplos
 [:nublado 20 91 :fuerte :contento :relajado :solvente :- ]
 [:soleado 28 86 :brisa :normal :relajado :solvente :+ ]
 [:lluvioso 29 70 :brisa :normal :normal :solvente :- ]
 [:niebla 22 80 :no :contento :estresado :solvente :+ ]
 [:diluvia 34 75 :no :normal :relajado :ajustado :- ]]
